'use strict';

// Reads .xlsx / .csv into plain header+row arrays for the importer.

const path = require('path');
const ExcelJS = require('exceljs');

// How far down the sheet to look for the header row. Contact lists often
// carry a title and a blank line before the real headers.
const HEADER_SEARCH_DEPTH = 10;

const { ROW_NUMBER } = require('../shared/importer');

/**
 * ExcelJS hands back rich text, hyperlink and formula objects as well as
 * plain values. Flatten all of them to the text a user would see in Excel.
 */
function cellToString(value) {
  if (value === null || value === undefined) return '';
  if (typeof value === 'string') return value.trim();
  if (typeof value === 'number' || typeof value === 'boolean') return String(value);
  if (value instanceof Date) return value.toISOString().slice(0, 10);
  if (Array.isArray(value)) return value.map(cellToString).join(' ').trim();

  if (typeof value === 'object') {
    // A mailto: hyperlink is the common case for an email column.
    if (value.text !== undefined) {
      const text = cellToString(value.text);
      if (text) return text;
      if (typeof value.hyperlink === 'string') return value.hyperlink.replace(/^mailto:/i, '').trim();
    }
    if (typeof value.hyperlink === 'string') return value.hyperlink.replace(/^mailto:/i, '').trim();
    if (Array.isArray(value.richText)) return value.richText.map((r) => r.text || '').join('').trim();
    // Formula cells: prefer the cached result over the formula itself.
    if (value.result !== undefined) return cellToString(value.result);
    if (value.error) return '';
  }
  return String(value).trim();
}

/**
 * Read a cell as the text the user sees in Excel.
 *
 * Site identifiers are routinely stored as numbers but displayed with leading
 * zeros through a "000" number format, and ExcelJS reports neither `.value`
 * nor `.text` with the padding applied. Losing it would turn site 001 into
 * site 1 and stop it matching anything else the trial holds, so the
 * zero-padding formats are applied here.
 */
function formatCell(cell) {
  const value = cell.value;
  if (typeof value === 'number' && Number.isInteger(value) && value >= 0) {
    const format = String(cell.numFmt || '').replace(/["';@]/g, '');
    if (/^0+$/.test(format) && format.length > 1) {
      return String(value).padStart(format.length, '0');
    }
  }
  return cellToString(value);
}

function rowValues(row, width) {
  const out = [];
  for (let col = 1; col <= width; col += 1) {
    out.push(formatCell(row.getCell(col)));
  }
  return out;
}

/**
 * Pick the header row: within the first few rows, the one with the most
 * filled cells that also has data underneath it.
 */
function findHeaderRow(grid) {
  let best = -1;
  let bestCount = 0;
  const limit = Math.min(grid.length, HEADER_SEARCH_DEPTH);
  for (let i = 0; i < limit; i += 1) {
    const filled = grid[i].filter((c) => c !== '').length;
    const hasDataBelow = grid.slice(i + 1).some((r) => r.some((c) => c !== ''));
    if (filled >= 2 && hasDataBelow && filled > bestCount) {
      best = i;
      bestCount = filled;
    }
  }
  return best;
}

/** Make header names unique and non-empty so they can key a row object. */
function normaliseHeaders(rawHeaders) {
  const seen = new Map();
  return rawHeaders.map((header, index) => {
    let name = String(header || '').trim();
    if (!name) name = `Column ${index + 1}`;
    const count = seen.get(name) || 0;
    seen.set(name, count + 1);
    return count === 0 ? name : `${name} (${count + 1})`;
  });
}

function sheetToTable(worksheet) {
  const width = Math.max(worksheet.columnCount || 0, 1);
  const grid = [];
  worksheet.eachRow({ includeEmpty: true }, (row) => {
    grid.push(rowValues(row, width));
  });

  // Drop trailing blank rows so the header heuristic is not confused by them.
  while (grid.length && grid[grid.length - 1].every((c) => c === '')) grid.pop();

  const headerIndex = findHeaderRow(grid);
  if (headerIndex === -1) {
    return { name: worksheet.name, headers: [], rows: [], firstDataRow: 2, rowCount: 0 };
  }

  const headers = normaliseHeaders(grid[headerIndex]);
  const dataRows = grid.slice(headerIndex + 1);

  // Ignore columns that are empty in both the header and every data row.
  const keptIndices = headers
    .map((_, i) => i)
    .filter((i) => grid[headerIndex][i] !== '' || dataRows.some((r) => r[i] !== ''));
  const usedHeaders = keptIndices.map((i) => headers[i]);

  const rows = [];
  dataRows.forEach((cells, offset) => {
    if (cells.every((c) => c === '')) return;
    const row = {};
    keptIndices.forEach((sourceIndex, hIndex) => {
      row[usedHeaders[hIndex]] = cells[sourceIndex] ?? '';
    });
    // Real spreadsheet row number, so warnings point at the row the user sees.
    // Non-enumerable so it is not mistaken for a data column downstream.
    Object.defineProperty(row, ROW_NUMBER, {
      value: headerIndex + 2 + offset,
      enumerable: false,
    });
    rows.push(row);
  });

  return {
    name: worksheet.name,
    headers: usedHeaders,
    rows,
    // Spreadsheet row number of the first data row, for actionable warnings.
    firstDataRow: headerIndex + 2,
    rowCount: rows.length,
  };
}

/**
 * Read every sheet of a workbook.
 *
 * @returns {Promise<{filePath: string, fileName: string, sheets: Array}>}
 */
async function readWorkbook(filePath) {
  const extension = path.extname(filePath).toLowerCase();
  const workbook = new ExcelJS.Workbook();

  if (extension === '.csv' || extension === '.tsv' || extension === '.txt') {
    await workbook.csv.readFile(filePath, {
      parserOptions: { delimiter: extension === '.tsv' ? '\t' : ',' },
      // ExcelJS converts anything number-like by default, which would turn
      // site "001" into 1. Every column here is treated as text.
      map: (datum) => datum,
    });
  } else if (extension === '.xlsx' || extension === '.xlsm') {
    await workbook.xlsx.readFile(filePath);
  } else if (extension === '.xls') {
    // ExcelJS cannot read the pre-2007 binary format, and guessing would give
    // a confusing parse error instead of an actionable one.
    const error = new Error(
      'This is an old-style .xls file. Open it in Excel and use File → Save As → Excel Workbook (.xlsx), then import again.',
    );
    error.code = 'LEGACY_XLS';
    throw error;
  } else {
    const error = new Error(`Unsupported file type "${extension}". Use .xlsx or .csv.`);
    error.code = 'UNSUPPORTED_TYPE';
    throw error;
  }

  const sheets = workbook.worksheets
    .map(sheetToTable)
    .filter((sheet) => sheet.headers.length > 0);

  if (sheets.length === 0) {
    const error = new Error('No readable table found. Check the sheet has a header row with column names.');
    error.code = 'NO_TABLE';
    throw error;
  }

  return { filePath, fileName: path.basename(filePath), sheets };
}

module.exports = { readWorkbook, sheetToTable, cellToString, findHeaderRow, normaliseHeaders };
