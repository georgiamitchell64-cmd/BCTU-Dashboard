'use strict';

// Writes sample/site-contacts-sample.xlsx: two sheets showing the two layouts
// the importer understands, including the messy cases it has to cope with.

const path = require('path');
const ExcelJS = require('exceljs');

async function main() {
  const workbook = new ExcelJS.Workbook();

  // One row per contact.
  const byContact = workbook.addWorksheet('Contacts by person');
  byContact.addRow(['TONIC — site contact list']); // stray title row above the headers
  byContact.addRow([]);
  byContact.addRow(['Site ID', 'Site Name', 'City', 'Status', 'Contact Name', 'Role', 'Email']);
  const contactRows = [
    ['001', 'Queen Elizabeth Hospital', 'Birmingham', 'Recruiting', 'Dr Jane Bloggs', 'Principal Investigator', 'jane.bloggs@uhb.nhs.uk'],
    ['001', 'Queen Elizabeth Hospital', 'Birmingham', 'Recruiting', 'Ade Okoro', 'Research Nurse', 'ade.okoro@uhb.nhs.uk'],
    ['001', 'Queen Elizabeth Hospital', 'Birmingham', 'Recruiting', 'Trial Office', 'Admin', 'qe.trials@uhb.nhs.uk'],
    ['002', 'Addenbrookes Hospital', 'Cambridge', 'Recruiting', 'Dr Sam Reed', 'Principal Investigator', 'sam.reed@addenbrookes.nhs.uk'],
    ['002', 'Addenbrookes Hospital', 'Cambridge', 'Recruiting', 'Priya Shah', 'Research Nurse', 'priya.shah@addenbrookes.nhs.uk'],
    ['003', 'Royal Infirmary', 'Edinburgh', 'Set-up', 'Dr Callum Fraser', 'Principal Investigator', 'c.fraser@nhslothian.scot.nhs.uk'],
    // Two addresses in one cell, which the importer splits.
    ['004', 'Freeman Hospital', 'Newcastle', 'Recruiting', 'Research Team', 'Research Nurse', 'r.team@nuth.nhs.uk; j.patel@nuth.nhs.uk'],
    // No address yet — the importer warns rather than failing.
    ['005', 'Southmead Hospital', 'Bristol', 'Set-up', 'TBC', 'Principal Investigator', ''],
    ['006', 'Royal Victoria Infirmary', 'Belfast', 'Paused', 'Dr Niamh Kelly', 'Principal Investigator', 'niamh.kelly@belfasttrust.hscni.net'],
  ];
  contactRows.forEach((row) => byContact.addRow(row));
  byContact.getRow(3).font = { bold: true };
  byContact.columns.forEach((column) => { column.width = 26; });

  // One row per site, several email columns.
  const bySite = workbook.addWorksheet('One row per site');
  bySite.addRow(['Centre No', 'Hospital', 'Status', 'PI Name', 'PI Email', 'Research Nurse', 'Research Nurse Email', 'Pharmacy Email', 'Randomised']);
  const siteRows = [
    ['001', 'Queen Elizabeth Hospital', 'Recruiting', 'Dr Jane Bloggs', 'jane.bloggs@uhb.nhs.uk', 'Ade Okoro', 'ade.okoro@uhb.nhs.uk', 'pharmacy.qe@uhb.nhs.uk', 24],
    ['002', 'Addenbrookes Hospital', 'Recruiting', 'Dr Sam Reed', 'sam.reed@addenbrookes.nhs.uk', 'Priya Shah', 'priya.shah@addenbrookes.nhs.uk', '', 18],
    ['003', 'Royal Infirmary', 'Set-up', 'Dr Callum Fraser', 'c.fraser@nhslothian.scot.nhs.uk', '', '', '', 0],
    ['004', 'Freeman Hospital', 'Recruiting', 'Dr Iwan Hughes', 'i.hughes@nuth.nhs.uk', 'Research Team', 'r.team@nuth.nhs.uk', 'pharmacy@nuth.nhs.uk', 11],
  ];
  siteRows.forEach((row) => bySite.addRow(row));
  bySite.getRow(1).font = { bold: true };
  bySite.columns.forEach((column) => { column.width = 26; });

  const target = path.join(__dirname, 'site-contacts-sample.xlsx');
  await workbook.xlsx.writeFile(target);
  process.stdout.write(`Wrote ${target}\n`);

  await writeReturnRates();
  await writeDataQueries();
}

// A return-rates export in the shape the dashboard produces, so the
// completeness fields can be tried without a real trial export.
async function writeReturnRates() {
  const workbook = new ExcelJS.Workbook();
  const sheet = workbook.addWorksheet('Return rates');
  sheet.addRow(['Site', 'Event', 'Form', 'Expected', 'Due', 'Entered',
    '% Due Entered', '% Expected Entered',
    'Queries Raised', 'Open Queries', 'Resolved Queries', 'Overdue Queries']);

  const sites = [
    { name: 'Queen Elizabeth Hospital', participants: 24, entry: 0.96, queries: [32, 3, 29, 0] },
    { name: 'Addenbrookes Hospital', participants: 18, entry: 0.88, queries: [21, 6, 15, 2] },
    { name: 'Freeman Hospital', participants: 11, entry: 0.71, queries: [14, 9, 5, 4] },
    { name: 'Royal Infirmary', participants: 6, entry: 1, queries: [4, 0, 4, 0] },
    { name: 'Southmead Hospital', participants: 9, entry: 0.62, queries: [11, 8, 3, 3] },
    { name: 'Royal Victoria Infirmary', participants: 4, entry: 0.93, queries: [5, 1, 4, 0] },
  ];
  // Later timepoints have fewer forms due: those windows have not all opened.
  const events = [
    { name: 'Baseline', dueShare: 1 },
    { name: 'Discharge', dueShare: 0.9 },
    { name: 'Day 30', dueShare: 0.65 },
    { name: 'Day 90', dueShare: 0.3 },
  ];
  const forms = ['Demographics', 'Bowel Function', 'Quality of Life'];

  const overall = new Map();
  for (const site of sites) {
    let first = true;
    for (const event of events) {
      for (const form of forms) {
        const expected = site.participants;
        const due = Math.round(expected * event.dueShare);
        const entered = Math.round(due * site.entry);
        const key = `${event.name}|${form}`;
        const running = overall.get(key) || { expected: 0, due: 0, entered: 0 };
        overall.set(key, {
          expected: running.expected + expected,
          due: running.due + due,
          entered: running.entered + entered,
        });
        sheet.addRow([
          site.name, event.name, form, expected, due, entered,
          due ? Math.round(entered / due * 100) : '',
          expected ? Math.round(entered / expected * 100) : '',
          // Queries are a site-level figure, so they sit on the first row only.
          ...(first ? site.queries : ['', '', '', '']),
        ]);
        first = false;
      }
    }
  }

  for (const [key, totals] of overall) {
    const [event, form] = key.split('|');
    sheet.addRow(['.Overall', event, form, totals.expected, totals.due, totals.entered,
      totals.due ? Math.round(totals.entered / totals.due * 100) : '',
      totals.expected ? Math.round(totals.entered / totals.expected * 100) : '',
      '', '', '', '']);
  }

  sheet.getRow(1).font = { bold: true };
  sheet.columns.forEach((column) => { column.width = 22; });

  const target = path.join(__dirname, 'return-rates-sample.xlsx');
  await workbook.xlsx.writeFile(target);
  process.stdout.write(`Wrote ${target}\n`);
}

// A data query export in the shape REDCap's Data Query Resolution report
// produces, so the query fields can be tried without a real export.
async function writeDataQueries() {
  const workbook = new ExcelJS.Workbook();
  const sheet = workbook.addWorksheet('Data queries');
  sheet.addRow(['ID (# of comments)', 'Site (DAG)', 'TNo', 'Event', 'Form/DQR', 'Instance',
    'Question', 'Data Category', 'Date opened', 'Opened by', 'First Comment',
    'Date of last comment', 'Last comment by', 'Last Comment', 'Query Status',
    'Assigned To', 'If OPEN, Duration', 'Responded & OPEN']);

  const sites = [
    { dag: 'queen_elizabeth_hospital', prefix: 'QE', open: 3, closed: 29 },
    { dag: 'addenbrookes_hospital', prefix: 'AD', open: 6, closed: 15 },
    { dag: 'freeman_hospital', prefix: 'FR', open: 9, closed: 5 },
    { dag: 'royal_infirmary', prefix: 'RI', open: 0, closed: 4 },
    { dag: 'southmead_hospital', prefix: 'SM', open: 8, closed: 3 },
    { dag: 'royal_victoria_infirmary', prefix: 'RV', open: 1, closed: 4 },
  ];
  const events = ['Baseline', 'Discharge', 'Day 30', 'Day 90'];
  const forms = ['Demographics', 'Bowel Function', 'Quality of Life', 'Adverse Events'];
  const questions = ['Date of birth is after the consent date',
    'Score is outside the expected range', 'Field left blank',
    'Two answers conflict', 'Units not recorded'];
  const categories = ['Missing data', 'Out of range', 'Inconsistent', 'Clarification'];

  const uk = (date) => `${String(date.getUTCDate()).padStart(2, '0')}-`
    + `${String(date.getUTCMonth() + 1).padStart(2, '0')}-${date.getUTCFullYear()}`;
  const daysAgo = (days) => new Date(Date.now() - days * 86400000);

  let id = 1000;
  for (const site of sites) {
    for (let i = 0; i < site.open + site.closed; i += 1) {
      const isOpen = i < site.open;
      // Open queries are spread across the age bands so the ageing chart has
      // something to show; the oldest land beyond the overdue threshold.
      const age = isOpen ? [3, 9, 12, 20, 26, 34, 41, 55, 68][i % 9] : 90 + i;
      const opened = daysAgo(age);
      const responded = isOpen && i % 3 === 0;
      sheet.addRow([
        `${id += 1} (${1 + (i % 3)})`,
        site.dag,
        `${site.prefix}${String(101 + i).padStart(3, '0')}`,
        events[i % events.length],
        forms[i % forms.length],
        (i % 2) + 1,
        questions[i % questions.length],
        categories[i % categories.length],
        uk(opened),
        'Trial team',
        'Please could you check and confirm this value.',
        uk(daysAgo(Math.max(0, age - 2))),
        responded ? 'Site' : 'Trial team',
        responded ? 'Checked, awaiting source data.' : 'Please could you check this.',
        isOpen ? 'Open' : 'Closed',
        isOpen ? 'Site' : 'Trial team',
        isOpen ? age : '',
        responded ? 'Yes' : '',
      ]);
    }
  }

  sheet.getRow(1).font = { bold: true };
  sheet.columns.forEach((column) => { column.width = 22; });

  const target = path.join(__dirname, 'data-queries-sample.xlsx');
  await workbook.xlsx.writeFile(target);
  process.stdout.write(`Wrote ${target}\n`);
}

main().catch((error) => {
  process.stderr.write(`${error.stack}\n`);
  process.exit(1);
});
