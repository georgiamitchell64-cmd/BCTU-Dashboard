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
}

main().catch((error) => {
  process.stderr.write(`${error.stack}\n`);
  process.exit(1);
});
