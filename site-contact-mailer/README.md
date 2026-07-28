# Site Contact Mailer

A desktop app for emailing trial site contacts. Import a contact list from
Excel, tick the sites you want, write the email in the app, and send it —
either as one message to everyone or as a personalised message per site.

It is a self-contained tool: it does not read from or write to the dashboard's
databases, and nothing leaves your machine unless you send an email.

## What it does

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

**Write** the message in the app. For a personalised send, use `{{fields}}`:

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

### Building an installer

```sh
npm run dist:win     # Windows installer + portable .exe
npm run dist:mac     # macOS .dmg
npm run dist:linux   # Linux AppImage
```

Installers are written to `dist/`. Build on the platform you are targeting;
cross-building from Linux to a signed Windows or macOS app needs extra tooling.

### Tests

```sh
npm test
```

Covers address parsing, column detection, the To/Bcc rule, template rendering
and the draft file format. `npm run sample` must have been run for the
workbook tests to do anything (they skip themselves otherwise).

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
    emails.js    address parsing, validation, de-duplication
    importer.js  column detection, grouping rows into sites
    compose.js   template rendering, the To/Bcc rule, the merge queue
    mailer.js    .eml drafts, mailto links, nodemailer payloads
  main/        Electron main process — the only code with disk/network access
    main.js      window, menu, IPC handlers, delivery
    workbook.js  reading .xlsx/.csv via ExcelJS
    store.js     persisted list, templates, settings, encrypted credentials
    preload.js   the IPC bridge exposed to the page
  renderer/    The user interface (no Node access)
```

The renderer runs with `contextIsolation` on, `nodeIntegration` off and a
restrictive content security policy; every privileged action goes through an
explicit channel in `preload.js`. Parsed spreadsheets stay in the main
process, so re-mapping columns re-runs the import there rather than shipping
whole sheets to the page.
