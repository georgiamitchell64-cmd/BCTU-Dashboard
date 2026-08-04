// ============================================================================
// TONIC trial data
// ----------------------------------------------------------------------------
// Demonstration figures shaped like the real REDCap export, pending a live
// feed. Everything the dashboard renders is derived from this object, so
// swapping in real data means replacing this file alone.
// ============================================================================

let TRIAL = {
  code: "TONIC",
  name: "TONIC",
  subtitle:
    "Multi-centre randomised trial of early parenteral nutrition after emergency laparotomy",
  isrctn: "59200516",
  target: 898,
  targetSites: 24,
  dataCut: "31 Jul 2026",
  firstRandomisation: "12 Mar 2026",
  targetClose: "Jun 2028",
  blinded: false,
};

// Recruitment by month. `target` is the protocol schedule from the trial config.
let MONTHS = [
  { label: "Mar 2026", short: "Mar", randomised: 3,  target: 4  },
  { label: "Apr 2026", short: "Apr", randomised: 7,  target: 8  },
  { label: "May 2026", short: "May", randomised: 10, target: 12 },
  { label: "Jun 2026", short: "Jun", randomised: 14, target: 12 },
  { label: "Jul 2026", short: "Jul", randomised: 13, target: 12 },
];

// Protocol recruitment schedule (cumulative target by month end), taken from
// the trial config. Drives the forecast chart beyond the data cut.
let TARGET_SCHEDULE = [
  { label: "Aug 2026", cumulative: 60  }, { label: "Sep 2026", cumulative: 76  },
  { label: "Oct 2026", cumulative: 96  }, { label: "Nov 2026", cumulative: 120 },
  { label: "Dec 2026", cumulative: 148 }, { label: "Jan 2027", cumulative: 180 },
  { label: "Feb 2027", cumulative: 216 }, { label: "Mar 2027", cumulative: 256 },
  { label: "Apr 2027", cumulative: 300 }, { label: "May 2027", cumulative: 348 },
  { label: "Jun 2027", cumulative: 398 }, { label: "Jul 2027", cumulative: 448 },
  { label: "Aug 2027", cumulative: 498 }, { label: "Sep 2027", cumulative: 548 },
  { label: "Oct 2027", cumulative: 598 }, { label: "Nov 2027", cumulative: 648 },
  { label: "Dec 2027", cumulative: 698 }, { label: "Jan 2028", cumulative: 748 },
  { label: "Feb 2028", cumulative: 798 }, { label: "Mar 2028", cumulative: 848 },
  { label: "Apr 2028", cumulative: 873 }, { label: "May 2028", cumulative: 898 },
];

// Projection assumptions, mirroring the Shiny dashboard's model: each month a
// few more sites open, and every open site recruits at a per-site rate.
//
// `ratePerSite` and `sitesPerMonth` below are only fallbacks. When there are at
// least three months of recruitment the app derives both from what has actually
// happened, the same way functions/projection_math.R does, and the sliders on
// the Recruitment page start from those derived values rather than these.
let PROJECTION = {
  targetSites: 24,
  ratePerSite: 0.75,      // participants per open site per month
  sitesPerMonth: 2.0,     // new sites opening per month
  // Optimistic and pessimistic are the central case bent by these factors.
  rateSpread: 0.2,        // ±20% on the per-site rate
  siteSpread: 1.0,        // ±1 site per month on the opening rate
};

// Site registry — maintained by hand, not derived from the data export.
//
// A REDCap export only evidences sites that have randomised someone, so a site
// in set-up or merely identified would be invisible if this list were built
// from participant records. Keeping the roster here means the denominator
// ("12 of 24 recruiting", the status doughnut, the dormant count) stays honest
// about sites that exist but have not yet contributed a participant.
//
// When a live feed is wired in, only `n` and `lastRand` should come from it,
// joined onto these rows by `name`; everything else stays manual. Add a new
// site the moment it is identified, with n: 0 and lastRand: null, and it will
// appear immediately with the right status.
//
// `lastRand` drives the dormant-site calculation (>60 days before the cut).
let SITES = [
  { name: "Queen Elizabeth Hospital Birmingham", region: "West Midlands", status: "Recruiting", opened: "Mar 2026", n: 8, lastRand: "28 Jul 2026" },
  { name: "University Hospital Coventry",        region: "West Midlands", status: "Recruiting", opened: "Mar 2026", n: 6, lastRand: "24 Jul 2026" },
  { name: "Royal Liverpool Hospital",            region: "North West",    status: "Recruiting", opened: "Apr 2026", n: 5, lastRand: "30 Jul 2026" },
  { name: "St James's Hospital Leeds",           region: "Yorkshire",     status: "Recruiting", opened: "Apr 2026", n: 5, lastRand: "19 Jul 2026" },
  { name: "Bristol Royal Infirmary",             region: "South West",    status: "Recruiting", opened: "Apr 2026", n: 4, lastRand: "26 Jul 2026" },
  { name: "Sheffield Teaching Hospital",         region: "Yorkshire",     status: "Recruiting", opened: "May 2026", n: 4, lastRand: "21 Jul 2026" },
  { name: "Oxford University Hospitals",         region: "South East",    status: "Recruiting", opened: "May 2026", n: 3, lastRand: "15 Jul 2026" },
  { name: "Newcastle Royal Victoria",            region: "North East",    status: "Recruiting", opened: "May 2026", n: 3, lastRand: "11 Jul 2026" },
  { name: "Southampton General Hospital",        region: "South East",    status: "Recruiting", opened: "Jun 2026", n: 3, lastRand: "27 Jul 2026" },
  { name: "Glasgow Royal Infirmary",             region: "Scotland",      status: "Recruiting", opened: "Jun 2026", n: 2, lastRand: "08 Jul 2026" },
  { name: "Nottingham University Hospital",      region: "East Midlands", status: "Recruiting", opened: "Jun 2026", n: 2, lastRand: "16 May 2026" },
  { name: "Addenbrooke's Hospital Cambridge",    region: "East",          status: "Recruiting", opened: "Jul 2026", n: 2, lastRand: "29 Jul 2026" },
  { name: "Royal Devon & Exeter Hospital",       region: "South West",    status: "Open",       opened: "Jul 2026", n: 0, lastRand: null },
  { name: "Aberdeen Royal Infirmary",            region: "Scotland",      status: "Open",       opened: "Jul 2026", n: 0, lastRand: null },
  { name: "University Hospital Wales Cardiff",   region: "Wales",         status: "Set-up",     opened: null,      n: 0, lastRand: null },
  { name: "Royal Sussex County Hospital",        region: "South East",    status: "Set-up",     opened: null,      n: 0, lastRand: null },
  { name: "Norfolk & Norwich Hospital",          region: "East",          status: "Set-up",     opened: null,      n: 0, lastRand: null },
  { name: "Derriford Hospital Plymouth",         region: "South West",    status: "Identified", opened: null,      n: 0, lastRand: null },
];

// Individual randomisations. Allocation is deliberately absent — the
// coordinating-centre view is blinded.
let RANDOMISATIONS = [
  { id: "TON-0047", site: "Royal Liverpool Hospital",            date: "30 Jul 2026", age: 71, sex: "F", nela: 8.4 },
  { id: "TON-0046", site: "Addenbrooke's Hospital Cambridge",    date: "29 Jul 2026", age: 66, sex: "M", nela: 5.1 },
  { id: "TON-0045", site: "Queen Elizabeth Hospital Birmingham", date: "28 Jul 2026", age: 78, sex: "F", nela: 14.2 },
  { id: "TON-0044", site: "Southampton General Hospital",        date: "27 Jul 2026", age: 59, sex: "M", nela: 3.8 },
  { id: "TON-0043", site: "Bristol Royal Infirmary",             date: "26 Jul 2026", age: 82, sex: "F", nela: 19.6 },
  { id: "TON-0042", site: "University Hospital Coventry",        date: "24 Jul 2026", age: 64, sex: "M", nela: 6.3 },
  { id: "TON-0041", site: "Sheffield Teaching Hospital",         date: "21 Jul 2026", age: 73, sex: "M", nela: 11.0 },
  { id: "TON-0040", site: "St James's Hospital Leeds",           date: "19 Jul 2026", age: 68, sex: "F", nela: 7.7 },
  { id: "TON-0039", site: "Queen Elizabeth Hospital Birmingham", date: "17 Jul 2026", age: 55, sex: "M", nela: 2.9 },
  { id: "TON-0038", site: "Oxford University Hospitals",         date: "15 Jul 2026", age: 80, sex: "F", nela: 16.4 },
  { id: "TON-0037", site: "Royal Liverpool Hospital",            date: "13 Jul 2026", age: 62, sex: "M", nela: 4.5 },
  { id: "TON-0036", site: "Newcastle Royal Victoria",            date: "11 Jul 2026", age: 75, sex: "F", nela: 12.8 },
  { id: "TON-0035", site: "University Hospital Coventry",        date: "09 Jul 2026", age: 69, sex: "M", nela: 8.9 },
  { id: "TON-0034", site: "Glasgow Royal Infirmary",             date: "08 Jul 2026", age: 57, sex: "F", nela: 3.2 },
  { id: "TON-0033", site: "Queen Elizabeth Hospital Birmingham", date: "06 Jul 2026", age: 84, sex: "M", nela: 21.3 },
  { id: "TON-0032", site: "Bristol Royal Infirmary",             date: "02 Jul 2026", age: 70, sex: "F", nela: 9.6 },
];

// Follow-up windows. `expected` counts participants who have reached the
// window given the data cut; `complete` counts forms received.
let FOLLOWUP = [
  {
    name: "Baseline", event: "baseline_arm_1", colour: "#0E6B5E",
    complete: 47, expected: 47,
    instruments: [{ label: "EQ-5D", complete: 47, expected: 47 }],
  },
  {
    name: "Discharge", event: "discharge_arm_1", colour: "#3FA593",
    complete: 43, expected: 45,
    instruments: [
      { label: "PRO-diGI", complete: 41, expected: 45 },
      { label: "EQ-5D",    complete: 43, expected: 45 },
      { label: "QoR-15",   complete: 42, expected: 45 },
    ],
  },
  {
    name: "Day 30", event: "day_30_arm_1", colour: "#7FA79F",
    complete: 34, expected: 38,
    instruments: [
      { label: "EQ-5D",        complete: 34, expected: 38 },
      { label: "Pat. Sat.",    complete: 31, expected: 38 },
      { label: "HRUQ",         complete: 30, expected: 38 },
    ],
  },
  {
    name: "Day 90", event: "day_90_arm_1", colour: "#B0AEA4",
    complete: 16, expected: 20,
    instruments: [
      { label: "EQ-5D", complete: 16, expected: 20 },
      { label: "HRUQ",  complete: 15, expected: 20 },
    ],
  },
];

// Per-site follow-up compliance (forms received / forms due across all windows).
let SITE_COMPLIANCE = [
  { name: "Queen Elizabeth Hospital Birmingham", complete: 24, due: 25 },
  { name: "University Hospital Coventry",        complete: 18, due: 19 },
  { name: "Royal Liverpool Hospital",            complete: 13, due: 15 },
  { name: "St James's Hospital Leeds",           complete: 15, due: 16 },
  { name: "Bristol Royal Infirmary",             complete: 11, due: 13 },
  { name: "Sheffield Teaching Hospital",         complete: 12, due: 12 },
  { name: "Oxford University Hospitals",         complete:  7, due:  9 },
  { name: "Newcastle Royal Victoria",            complete:  8, due:  9 },
  { name: "Southampton General Hospital",        complete:  6, due:  7 },
  { name: "Glasgow Royal Infirmary",             complete:  4, due:  6 },
  { name: "Nottingham University Hospital",      complete:  5, due:  6 },
  { name: "Addenbrooke's Hospital Cambridge",    complete:  3, due:  3 },
];
