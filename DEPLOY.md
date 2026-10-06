# Deploying xDateR: a note for Kevin

This note explains how to put the new version of xDateR on the Posit Connect
server on your end. You do not need to run R or change any
code. Everything here happens in the Connect web page.

I think I have the lingo right. Or close to it.

Recall, the live app is at <https://viz.datascience.arizona.edu/xDateR/>.

## The short version

1. In Connect, make a second xDateR that is built from the new code. It sits
   beside the live app at its own address, and only people you send the
   address to will see it. (Step 1 below says which buttons to press.)
2. Check that this test copy works (five minutes, list below).
3. Tell me. I move the new code to where the live app looks for it, and
   the live app updates itself.

Until step 3 the live app is not touched. Nothing in steps 1 and 2 can
break it.

## Two words used below

- **Branch.** The repository holds two versions of the code side by side.
  The `main` branch is the version the live app runs. The branch
  called `dplr-1.8.0` is the new version. It is the same idea as a branch in
  any git project you have used.
- **Content.** Connect's word for one thing it hosts. The live xDateR is one
  piece of content. The test copy you make in step 1 will be another. They
  do not share anything, so the test copy can fail or be deleted without the
  live app noticing.

## How deployment works here

Connect watches a branch of the GitHub repository
<https://github.com/OpenDendro/xDateR>. When a new commit arrives on that
branch, Connect downloads the code, installs the R packages the app needs
and restarts the app. Nobody uploads anything by hand.

Connect learns which packages to install from one file in the repository,
`manifest.json`. It is a packing list: the version of R, and each package
with its exact version. Connect reads only this file. I already made
a new `manifest.json` for the new version, and it is on the branch.

## What changed, and why it needs care

- The app now needs >= **dplR 1.8.0**. The live app runs dplR 1.7.8.
- The packing list is shorter. Nothing new was
  added.
- The new `manifest.json` was made on a Mac. The old notes in the README say
  it must be made on Linux. I think that is not needed, but have not
  tested it on this server. **The test copy in step 1 is that test.**

## Step 1: make the test copy

First, look at how the live app is set up, so you know what you are copying:

1. Sign in to Connect and open the **Content** page.
2. Find xDateR in the list and open it. Its description should say
   "from Git".
3. Open **Settings**, then the **Source** panel. Under **Git Details** it
   shows the repository and the branch the live app is built from. The
   branch should be `main`. Change nothing here.

Now make the test copy:

1. Go back to the **Content** page.
2. Click **Publish**, then **Import from Git**.
3. Repository URL: `https://github.com/OpenDendro/xDateR`
4. Branch: choose `dplr-1.8.0` from the list.
5. Directory: Connect lists the folders that hold a `manifest.json`. There is
   one, the top level. Choose it.
6. Title: `xDateR test`, or anything that cannot be mistaken for the live
   app.
7. Click **Deploy Content**.

Connect now builds the test copy. This takes several minutes the first time,
because it installs every package. The build log is on the screen. When it
finishes, Connect shows the app and its address.

You need the "publisher" role in Connect to see the **Publish** button. If
the button is not there, that is the reason, and the server's administrator
can grant it.But I think you won't need that.

## Step 2: check the test copy

Open the test copy and try these. Each takes under a minute.

1. The page loads and shows the welcome screen.
2. In the sidebar, click the link that starts **Try the example data**. The
   Overview fills in.
3. Open the **About** section at the bottom of the sidebar. It should read
   `dplR 1.8.0`.
4. Click the **Correlations** tab. A grid of coloured tiles appears.
5. On the Overview, click **Generate report**. An HTML page
   downloads and opens. This checks that the report software on the server
   works.
6. Click the **Floater** tab. Under Undated Series in the sidebar, click the
   link that starts **Try the example data**. A plot appears.

If all six work, the server can run the new version.

## Step 3: tell me

I then merge `dplr-1.8.0` into `main`. Connect checks for new code every 15
minutes and then rebuilds the live app. To skip the wait, open the live
xDateR, go to **Settings**, **Source**, and click **Update Now**. Then run
the six checks again on the live address.

The test copy can then be deleted in Connect, or kept for the next update.

## If the build fails

Do not try to fix it. The live app is unaffected. Send me:

- the last 30 or so lines of the build log, and
- the version of R the server offers (shown near the top of the log).

The likely causes, so the log makes sense:

| The log says | What it means |
|---|---|
| A package "is not available" or fails to install | The server could not get that version of a package. Andy adjusts `manifest.json`. |
| R version does not match | The manifest asks for R 4.5. The server needs an R 4.5.x installed. |
| `xDateR needs dplR 1.8.0 or later` | The packages installed, but from the old packing list. The branch or the manifest is not the new one. |

## One setting worth checking

Open the live xDateR in Connect, go to **Settings**, then the **Runtime**
tab, and find the timeout settings. Please tell Andy the values of
**Connection timeout** and **Read timeout**. Do not change them yet.

Why it matters: people using xDateR look away from the screen for long
stretches while they check the wood. If Connect closes a session that has
been quiet too long, their unsaved edits are lost without warning.

Tyson did the previous deployment and may remember
details of the server that are not written down here.
