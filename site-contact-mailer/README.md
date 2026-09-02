# Site Contact Mailer

A desktop app for emailing trial site contacts. Import a contact list from
Excel, tick the sites you want, write the email in the app, and send it —
either as one message to everyone or as a personalised message per site.

It is a self-contained tool: it does not read from or write to the dashboard's
databases, and nothing leaves your machine unless you send an email.

## What it does

**Work on several messages at once.** New email starts a fresh draft; whatever
you were writing is saved automatically (subject, message, ticked sites, send
mode) and listed in the Drafts sidebar, so you can park a monthly-update email
half-written, switch to a quick questionnaire chase, and come back to the
first later — the app reopens on whichever draft you touched last. Sending a
draft removes it from the list, the way it leaves an Outlook Drafts folder
once it's gone out.

**Import** an `.xlsx` or `.csv` contact list. The app works out which columns
are which and shows you the result before anything is saved. Two layouts are
understood:

| Layout | Looks like |
| --- | --- |
| One row per contact | `Site ID · Site Name · Contact Name · Role · Email` |
| One row per site | `Centre No · Hospital · PI Name · PI Email · Nurse Email · …` |

A cell holding several addresses (`a@nhs.net; b@nhs.net`) is split into
separate contacts, and a title row above the headers is skipped. Anything the
app could not read is listed as a warning with the row number, rather than
being dropped silently.

**Address** the email by ticking sites. The recipient rule is applied for you:

- **One site selected** → its contacts go in the **To** line.
- **More than one site** → everyone goes in **Bcc**, so sites cannot see each
  other's addresses, and your own address goes in **To**.

Expand a site to untick individual people. The bar above the subject always
shows exactly who will be addressed and how.

**Choose who at each site** with the **Send to** chips. By default everyone at
a selected site is written to; picking roles narrows it to just those people —
only the PIs, only R&D, or the research teams. Contacts outside the chosen
roles are struck through in the list rather than silently dropped, and a site
with nobody in those roles is called out as skipped.

Roles are grouped as they are read, so the many ways a spreadsheet writes the
same job all land together: `PI`, `P.I.`, `Principal Investigator` and
`Chief Investigator` become one **Principal Investigator** chip, and `R&D`,
`R & D` and `Research and Development` become one **R&D** chip. Anything not
recognised keeps its own wording and gets its own chip.

**Write** the message with formatting — bold, italic, underline, fonts, sizes,
text colour, highlighting, bullets, numbering, alignment, indents, links and
tables. Pasting from Outlook or a webpage **keeps its formatting**: the markup
is cleaned of Word's `mso-` cruft and stripped of anything executable, but the
styling survives. `Load HTML…` pulls in a `.html` or `.eml` file as the
message, and `</> Source` exposes the raw HTML for a message written
elsewhere. **Attach file…** adds attachments to every email in the send.

Formatted messages are sent as `multipart/alternative` — the HTML *and* a
plain-text copy generated from it — so they read correctly on restricted mail
clients, and their absence is a common reason for a message to score as spam.
Images pasted into the body travel as proper inline attachments rather than
`data:` URIs, which most mail clients refuse to render.

For a personalised send, use `{{fields}}`:

```
Subject: {{site_name}} — recruitment update

Dear {{first_name|colleagues}},

{{site_name}} (site {{site_id}}) has randomised {{randomised|0}} participants.
```

`{{first_name|colleagues}}` means "use the first name, or the word
*colleagues* if it is blank". Every column in your spreadsheet is available as
a field, alongside built-ins like `{{site_name}}`, `{{site_id}}` and
`{{today}}` — use the **Insert field** menu to see the full list. Subject and
message can be saved as a template for next time.

Placeholders keep working inside formatted text. Bolding half of a
`{{site_name}}` splits it across tags, which would otherwise leave the raw
`{{site_name}}` in the sent email; the app repairs that before merging.

### Recruitment charts

**Recruitment data…** in the toolbar imports a randomisation export, which
unlocks a set of chart and figure fields for the message. Four layouts are
recognised, so most trial exports import without being reshaped first:

| Layout | Looks like |
| --- | --- |
| Months across columns | `Site · Opened · Total · Apr-2026 · May-2026 · …` |
| One row per participant | `Participant ID · Site · Randomisation Date` |
| One row per site per month | `Site · Month · Randomised` |
| One row per site | `Site · Randomised · Target` |

A row of machine codes above the real headers is skipped, a trailing `Total`
row is not counted as a site, and `-` (the site was not open that month) is
kept distinct from `0` (open, recruited nobody). Dates are read as UK format,
so `07-04-2026` is April, not July. If the file's own total disagrees with its
monthly columns, the app says so rather than quietly picking one.

The charts are `{{recruitment_chart}}` (all sites ranked, the recipient's own
highlighted), `{{progress_chart}}` (their progress against target),
`{{trend_chart}}` (their recruitment by month) and `{{overall_chart}}`
(the whole trial). Alongside them are plain figures — `{{site_randomised}}`,
`{{site_rank_of}}`, `{{site_percent}}`, `{{trial_randomised}}` and others.

**Sites are named.** The recipient sees their own site highlighted and the
rest named alongside it, which is what makes a league table worth reading.
Settings turns that off: *Anonymise other sites in the recruitment chart*
shows everyone but the recipient as "Site A", "Site B", so nobody is
identified to their peers as the site furthest behind.

Charts are built from HTML tables with `bgcolor` and pixel widths, not images
or JavaScript, because Outlook renders mail through Word and blocks both. They
survive in the plain-text alternative as a readable list.

If your randomisation export has no target column — many do not — the progress
chart falls back to a `Target` column in your **contact list**, so you can keep
targets there without editing the trial's export.

The **Monthly recruitment update** template under *Templates → Ready-made*
puts all of this together, and only appears once recruitment data is loaded.

### Data completeness and queries

**Completeness data…** in the toolbar imports the return-rates export — the
same file the TONIC dashboard reads — and unlocks a second set of fields and
charts, for the trophy for the most complete data. Three layouts are read:

| Layout | Looks like |
| --- | --- |
| One row per site, event and form | `Site · Event · Form · Expected · Due · Entered · % Due Entered` |
| One row per site | `Site · Forms Due · Forms Entered` |
| One row per site, percentage only | `Site · % Complete` |

`.Overall` rows are taken as the trial figure rather than counted as a site,
and a site with nothing due yet is left out of the league table rather than
being ranked last on a rate it had no chance to earn.

**Rates are calculated against forms due, not forms expected.** "Expected"
counts every form a participant will eventually reach, including windows that
have not opened — Day 90 for someone randomised last week — which would
deflate the rate. Forms entered before their window opens are capped at due,
so a site cannot come out above 100%. Both choices match the dashboard, so the
figure in an email agrees with the figure on screen. *Rate calculated against*
in the import dialog switches to expected if your trial reports it that way.

The charts are `{{completeness_chart}}` (all sites ranked, the recipient's own
highlighted), `{{completeness_leaderboard}}` (the league table, with places
gained or lost since the last import), `{{completeness_gauge}}` (the recipient
against the trial average and the leader), `{{completeness_event_chart}}`
(their completeness by timepoint), `{{completeness_form_chart}}` (the forms
they are furthest behind on), `{{completeness_trend_chart}}` (their own
history) and `{{overall_completeness_chart}}` (the whole trial).

Alongside them are the competitive figures — `{{completeness}}`,
`{{completeness_position}}`, `{{completeness_gap_to_top}}`,
`{{completeness_gap_to_next}}`, `{{completeness_vs_average_words}}`,
`{{completeness_movement}}` and `{{completeness_trophy}}` — plus
`{{completeness_headline}}`, which assembles the lot into one sentence:

> 71% of your due forms are entered, which puts Freeman Hospital 5th of 6
> sites, 15.7 points below the trial average.

**Movement needs two imports.** Each import is snapshotted, and the next one
compares against it, which is where "up two places since the last update"
comes from. The first import has no movement to report and says nothing
rather than inventing a change. *Remove imported data* keeps the snapshots, so
re-importing later picks the history back up.

**Data queries** come from the same file. If the export carries query columns
— raised, open, resolved, overdue — they are read too and add
`{{quality_scorecard}}` (completeness and queries in one table),
`{{query_chart}}`, `{{query_breakdown_chart}}`, `{{queries_open}}`,
`{{queries_overdue}}`, `{{query_position}}` and `{{query_headline}}`. Sites
are ranked on the *share* of their queries closed rather than the number
outstanding, so a large site is not permanently bottom for being large. If the
export has no query columns, none of these fields is offered.

**Naming other sites in charts** in Settings applies to the completeness and
query charts: name every site (the default), name the leading three only — the
race is public, the tail is not — or name only the recipient's own.

Three more ready-made templates appear once the data is loaded: **Data
completeness update**, **Completeness league table (trophy chase)** and
**Monthly update: recruitment and completeness**.

**Send** one of three ways:

| Mode | What happens | When to use it |
| --- | --- | --- |
| **Draft in my email app** (default) | Writes `.eml` draft files and opens them. Outlook opens them as normal editable drafts with a Send button. | Almost always. Sends from your own mailbox, with your signature, and lands in Sent Items. Handles long messages. |
| **Open a compose window** | Hands the addresses to your default mail program over a `mailto:` link. | Quick, short messages. The app warns you if the message is too long to survive the link. |
| **Send directly (SMTP)** | Sends immediately from the app. | Only if you have a server that will accept your account. No chance to review first. |

Nothing is ever sent without you confirming. In the two drafting modes the
app only prepares the message — you still press Send in your email program.

### One email, or one per site

The **One email** / **One per site** switch above the subject controls this.

- *One email* — a single message, addressed per the To/Bcc rule above.
- *One per site* — a separate message per site, each addressed **To** that
  site's own contacts with its own rendered subject and body. Tick *a separate
  email to each individual contact* to go one step further and send each
  person their own copy, which is what you want when the message opens with
  `Dear {{first_name}}`.

Use **Preview** to page through exactly what will be sent before sending it.

## Running it

```sh
npm install
npm start
```

To try it without a real contact list, generate the sample workbook first and
import `sample/site-contacts-sample.xlsx`:

```sh
npm run sample
```

That also writes `sample/return-rates-sample.xlsx`, for the completeness and
query fields.

### Building an installer

```sh
npm run dist
```

Written to `dist/`:

| File | What it is |
| --- | --- |
| `Site Contact Mailer Setup <version>.exe` | The installer. Installs per-user, so no admin rights are needed. |
| `site-contact-mailer-<version>-win.zip` | The same app with no installer. Unzip it anywhere and run `Site Contact Mailer.exe` — the option to reach for on a managed machine that blocks installers. |
| `Site Contact Mailer <version>.exe` | A single portable executable. |

Windows is the only target configured, and the build must run on Windows —
cross-building from Linux needs wine and gains nothing here.

**Or let GitHub build it.** The *Mailer build (Windows)* workflow runs on every
push that touches the app, and can be started by hand from the Actions tab. It
runs the tests, builds all three, and attaches them to the run as an artifact
called `Site-Contact-Mailer-Windows` — which downloads as a zip. That saves
needing a Windows machine with Node on it.

**On a managed Windows machine**, the build config sets
`win.signAndEditExecutable: false`. Without it, electron-builder downloads its
`winCodeSign` toolkit, which contains macOS symlinks, and the extraction fails
with *"Cannot create symbolic link: A required privilege is not held by the
client"* — creating symlinks on Windows needs a privilege that a standard
account does not have. Turning the option off skips that download entirely.

That toolkit is also what electron-builder uses to write an icon and version
information into the `.exe`, so switching it off would normally cost you those.
It does not here — see below.

### The icon

The icon is drawn as SVG in `tools/make-icon.js` and rendered by:

```sh
npm run icon
```

An envelope in the same navy as the planner but in this app's green, so the two
read as a pair in the taskbar without being mistaken for each other. Changing it
means editing the shape in that script and re-running, rather than hunting for
whatever tool produced a binary.

| File | Used for |
| --- | --- |
| `build/icon.ico` | Windows — the `.exe`, its shortcuts, and the installer |
| `build/icon.png` | The 1024×1024 master render |
| `src/assets/icon.png` | Shipped inside the app; `BrowserWindow` uses it for the window and taskbar icon at runtime |

Stamping an icon into a Windows executable is `rcedit`'s job, and
electron-builder only runs it out of the `winCodeSign` toolkit we disabled
above. So `build/after-pack.js` runs `rcedit` directly instead: the `rcedit` npm
package ships `rcedit.exe` on its own, with no archive and no symlinks, so it
needs no special privilege. Built on Windows, the `.exe`, its shortcuts, the
installer and the running window all carry the icon.

The hook is best-effort by design: if `rcedit` is missing or fails, it warns and
the build carries on. A wrong icon is a blemish, a failed build is not. The one
case where it does fail is cross-building from Linux, where `rcedit.exe` needs
wine.

### Tests

```sh
npm test
```

Covers address parsing, column detection, role grouping and filtering, the
To/Bcc rule, template rendering in both plain text and HTML, the paste
sanitiser, the MIME structure of the generated drafts, and — for completeness
— the due-not-expected denominator, the 100% cap, ranking and ties, movement
between imports, and that every advertised merge field is actually produced. `npm run sample`
must have been run for the workbook tests to do anything (they skip
themselves otherwise).

## Notes for the trial office

**Old `.xls` files.** ExcelJS cannot read the pre-2007 binary format. Open the
file in Excel and use *File → Save As → Excel Workbook (.xlsx)*. The app tells
you this rather than failing with a parse error.

**Where your data lives.** The imported contact list, saved templates and
settings are stored in a JSON file in your user profile — the exact path is
shown at the bottom of the Settings dialog. Nothing is uploaded anywhere. Use
**Export** to write the current list back out as CSV.

**Re-importing.** By default a new import replaces the list. Tick *add to the
existing list* to merge instead, which keeps sites you already have, adds new
addresses, and preserves any contacts you had unticked.

**Bcc and the To line.** A message with an empty To line is more likely to be
treated as spam, so when everyone is bcc'd the app puts your own address in
To. Set it in Settings; the app warns you before sending if it is missing.

**SMTP passwords.** If you do configure direct sending, the password is
encrypted with your operating system's keychain (via Electron `safeStorage`).
If no keychain is available the password is not saved to disk at all, and you
will be asked for it again each session.

## How it fits together

```
src/
  shared/      Pure logic, no Electron — unit tested directly
    emails.js      address parsing, validation, de-duplication
    importer.js    column detection, role grouping, rows into sites
    recruitment.js randomisation layouts, monthly totals, ranking
    completeness.js return-rate layouts, per-site rates, league table, queries
    charts.js      email-safe HTML charts (tables, no JS, no images)
    templates.js   the ready-made messages shipped with the app
    compose.js     template rendering, To/Bcc, role filtering, merges
    html.js        paste cleaning, sanitising, HTML->text, HTML placeholders
    mailer.js      MIME drafts, mailto links, nodemailer payloads
  main/        Electron main process — the only code with disk/network access
    main.js      window, menu, IPC handlers, attachments, delivery
    workbook.js  reading .xlsx/.csv via ExcelJS
    store.js     persisted list, templates, drafts, settings, encrypted
                 credentials, and the completeness snapshots behind "up two
                 places since last month"
    preload.js   the IPC bridge exposed to the page
  renderer/    The user interface (no Node access)
    editor.js    the rich-text editor and its paste handling
```

`shared/html.js` is loaded both ways: `require()`d by the main process, and
included as a plain `<script>` in the renderer, because the sanitiser has to
run synchronously as the user types and pastes.

The renderer runs with `contextIsolation` on, `nodeIntegration` off and a
restrictive content security policy; every privileged action goes through an
explicit channel in `preload.js`. Parsed spreadsheets stay in the main
process, so re-mapping columns re-runs the import there rather than shipping
whole sheets to the page.

The policy does allow `'unsafe-inline'` for styles, which rich-text editing
requires — the editor produces inline `style` attributes and pasted email
carries its own. Every piece of such HTML goes through the sanitiser first,
scripts and event handlers are removed, and the renderer has no Node access,
so the remaining exposure is limited to the page's own appearance.
