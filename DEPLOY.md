# Deploying xDateR: a note for Kevin

This note explains how to put the new version of xDateR on the Posit Connect
server at the University of Arizona. You do not need to run R or change any
code. Everything here happens in the Connect web page.

The live app is at <https://viz.datascience.arizona.edu/xDateR/>.

## The short version

1. Publish a second, test copy of the app from the branch `dplr-1.8.0`.
2. Check that the test copy works (five minutes, list below).
3. Tell Andy. He merges the branch into `main`, and the live app updates
   itself.

Until step 3 the live app is not touched. Nothing in steps 1 and 2 can
break it.

## How deployment works here

Connect watches a branch of the GitHub repository
<https://github.com/OpenDendro/xDateR>. When a new commit arrives on that
branch, Connect downloads the code, installs the R packages the app needs
and restarts the app. Nobody uploads anything by hand.

Connect learns which packages to install from one file in the repository,
`manifest.json`. It is a packing list: the version of R, and each package
with its exact version. Connect reads only this file. Andy has already made
a new `manifest.json` for the new version, and it is on the branch.

## What changed, and why it needs care

- The app now needs **dplR 1.8.0**. The live app runs dplR 1.7.8.
- The packing list is shorter: 99 packages, down from 141. Nothing new was
  added.
- The new `manifest.json` was made on a Mac. The old notes in the README say
  it must be made on Linux. We think that is not needed, but we have not
  tested it on this server. **The test copy in step 1 is that test.**

## Step 1: publish a test copy from the branch

In the Connect web page:

1. Sign in and choose **Publish**, then **Import from Git**.
2. Repository: `https://github.com/OpenDendro/xDateR`
3. Branch: `dplr-1.8.0`
4. Directory: the top level (the one that holds `manifest.json`).
5. Give it a name that cannot be mistaken for the live app, such as
   `xDateR-test`, and deploy.

Connect now builds the app. This takes several minutes the first time,
because it installs every package. The build log is on the screen.

If the menu names differ from these, look at how the live xDateR is set up
(open it in Connect and look at its **Info** panel) and copy that, changing
only the branch and the name.

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

## Step 3: tell Andy

Andy merges `dplr-1.8.0` into `main`. Connect notices within a few minutes
and rebuilds the live app. Run the six checks again on the live address.

The test copy can then be deleted in Connect, or kept for the next update.

## If the build fails

Do not try to fix it. The live app is unaffected. Send Andy:

- the last 30 or so lines of the build log, and
- the version of R the server offers (shown near the top of the log).

The likely causes, so the log makes sense:

| The log says | What it means |
|---|---|
| A package "is not available" or fails to install | The server could not get that version of a package. Andy adjusts `manifest.json`. |
| R version does not match | The manifest asks for R 4.5. The server needs an R 4.5.x installed. |
| `xDateR needs dplR 1.8.0 or later` | The packages installed, but from the old packing list. The branch or the manifest is not the new one. |

## One setting worth checking

Open the live app's **Access** or **Runtime** settings in Connect and find
the idle timeout (how long a session with no activity is kept). People
using xDateR look away from the screen for long stretches while they check
the wood, and a session that times out loses their unsaved edits without
warning. If the timeout is under an hour, please raise it, or tell Andy what
it is.

## Who to ask

Andy Bunn wrote the app. Tyson did the previous deployment and may remember
details of the server that are not written down here.
