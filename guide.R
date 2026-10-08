# The guided example: one complete crossdating job on the example data,
# as a checklist in the sidebar. Each step ticks itself off when the user
# has done it (the conditions are in server.R, next to the state they read).
#
# The example data are dplR's co021 with three planted errors (see
# xDaterAppStuff/maketestData.R):
#   ABC118 (co021 645221): the 1797 ring deleted          -> a missing ring
#   ABC104 (co021 643143): the 1425 ring duplicated       -> a false ring
#   ABC110 (undated file): the 1816 ring duplicated, and the last 21 years
#     cut off, for the Floater panel (the second guide, floaterSteps below)
# The guide's answers undo the first two exactly: tests/test-server.R checks
# that following them gives back the co021 series.
#
# Each step: id, the panel it happens on (tab), the series to select when
# the user asks to be taken there, the year to put the Edit window on, a
# title and the instruction (HTML). read = TRUE marks a step that is about
# reading what is on the panel: it is finished with the guide's Next button.
# The others are things to do, and finish when the app sees them done.
guideSteps <- list(
  list(id = "checks", tab = "OverviewTab", series = NULL, year = NULL, read = TRUE,
       title = "Read the Data Checks",
       text = paste(
         "The Overview's <b>Data Checks</b> say what needs a look. Here they say",
         "ABC118 may be misdated by a year, and that segments of ABC104 and",
         "ABC118 are flagged. Next, the <b>Correlations</b> panel shows where.")),
  list(id = "find", tab = "AllSeriesTab", series = NULL, year = NULL,
       title = "Find the problem",
       text = paste(
         "Each series is tested against the others in 50-year segments.",
         "<b>Purple</b> segments fit better at another lag; hover over one to see",
         "the lag. Click one of <b>ABC118</b>'s purple segments (or its row in",
         "Flagged Segments) to open that series.")),
  list(id = "look", tab = "IndividualSeriesTab", series = "ABC118", year = 1797, read = TRUE,
       title = "Look at the series",
       text = paste(
         "Two views of ABC118 against the master. In the upper plot its",
         "correlation collapses before about 1800. In the lower plot the peak in",
         "those segments sits at lag &minus;1, not 0: those rings are dated a year",
         "late, the sign of a missing ring. Next, the <b>Edit</b> panel.")),
  list(id = "fix", tab = "EditSeriesTab", series = "ABC118", year = 1797,
       title = "Fix it",
       text = paste(
         "The card under the skeleton plot (Edit tab) says where to look: a missing ring in",
         "1755&ndash;1804. The statistics can't say which ring; for that you would",
         "go back to the wood. In this example the answer is known: the 1797 ring",
         "was left out. In the Measurements table click the row for <b>1798</b>,",
         "set the ring width to <b>0.25</b>, and click <b>Insert Above Selected",
         "Row</b> (leave Fix Last Year checked).")),
  list(id = "confirm", tab = "OverviewTab", series = NULL, year = NULL, read = TRUE,
       title = "Confirm the fix",
       text = paste(
         "The card on the Edit panel now compares the series <i>as loaded</i> with",
         "<i>now</i>: every segment fits where dated. On the <b>Overview</b>,",
         "Data Checks lists what your edit resolved.")),
  list(id = "own", tab = "IndividualSeriesTab", series = "ABC104", year = 1425,
       title = "Now one on your own: ABC104",
       text = paste(
         "ABC104 is flagged too, at lag +1 this time. Find where to look, fix it,",
         "and confirm, as you did for ABC118. A hint: a lag of +1 before correctly",
         "dated segments means a <i>false</i> ring, so this fix is a delete."),
       answer = paste(
         "Rings 1424 and 1425 of ABC104 are both 0.20 mm: 1425 is a duplicate.",
         "On the Edit panel, with ABC104 selected, click the row for <b>1425</b>",
         "and click <b>Delete Selected Row</b>.")),
  list(id = "save", tab = "EditSeriesTab", series = NULL, year = NULL,
       title = "Save your work",
       text = paste(
         "Your edits exist only in this session. Download the <b>edited file</b>",
         "(in the sidebar, or on the Edit panel) and, on the Edit panel, generate",
         "the <b>edit report</b>: its R code replays your edits on the original",
         "file, so the result can be reproduced."))
)

guideFinished <- paste(
  "You did the job: find, understand, fix, confirm, save. On real data",
  "the statistics tell you where to look but only the wood tells you what really happened.")

# ── The Floater guide ─────────────────────────────────────────────────────────
# Dating the undated example series against the example master. ABC119 is a
# clean case (r = 0.83 at 1466-1659, next best 0.35). ABC110 dates clearly
# too (r = 0.67 at 1337-1943) but has the duplicated 1816 ring inside it,
# which the segment table shows as a run of lag -1 segments from 1805.
# In the dated file the duplicate shows as 1816 = 1817 = 0.50 mm; deleting
# 1817 with Fix Last Year UNCHECKED clears every flag (checked, it makes
# things worse: a floater is placed by its older part, so the first year is
# the one to keep).
#
# Each step: id, the series to select on the Floater panel (series2), read
# (as above), a title and the instruction. Every step is on the Floater
# panel.
floaterSteps <- list(
  list(id = "load", series2 = NULL,
       title = "Load the undated series",
       text = paste(
         "The Floater panel dates series whose years are not known by sliding",
         "them along the master. Click <b>Try the example data</b>, just above",
         "this guide.")),
  list(id = "fit", series2 = "ABC119", read = TRUE,
       title = "Read the best fit",
       text = paste(
         "ABC119 is tried at every position along the master. The lower plot",
         "shows the correlation at each one; the best is 0.83, for 1466&ndash;1659.",
         "<b>Candidate positions</b> lists the next best too: it correlates at",
         "only 0.35, so the fit is clear-cut. If the two were close, the dating",
         "would be ambiguous, and you could choose between them there.")),
  list(id = "segs", series2 = "ABC119", read = TRUE,
       title = "Check it segment by segment",
       text = paste(
         "A good overall fit can hide a problem inside the series. <b>Segments at",
         "These Dates</b> tests each segment of the placed series, as the",
         "Correlations panel does. For ABC119 every segment fits best where dated.")),
  list(id = "save", series2 = "ABC119",
       title = "Save the dates",
       text = paste(
         "You are satisfied with ABC119. Click <b>Save These Dates</b>; the date",
         "log records it.")),
  list(id = "pick", series2 = NULL,
       title = "Now a harder one: ABC110",
       text = paste(
         "Under <b>Choose undated series</b>, select <b>ABC110</b>.")),
  list(id = "inside", series2 = "ABC110", read = TRUE,
       title = "A problem inside the floater",
       text = paste(
         "ABC110 dates clearly too: 0.67 for 1337&ndash;1943, next best 0.28. But",
         "its segments from 1805 on fit better a year earlier. A floater is placed",
         "by the part that fits, here its older part, so the rings <i>after</i> the",
         "error are the ones that are off: the summary says to look for a",
         "<i>false</i> ring in 1805&ndash;1854. The dates are right; the series",
         "needs a fix.")),
  list(id = "download", series2 = "ABC110",
       title = "Save and download",
       text = paste(
         "Click <b>Save These Dates</b> for ABC110 as well (the date log notes the",
         "lagged segments), tick <b>Append the master series?</b>, and click",
         "<b>Download dated .rwl</b>."))
)

floaterFinished <- paste(
  "Both series are dated. To finish the job on ABC110, load the file you just",
  "downloaded as a dated file and open ABC110 on the Edit panel: rings 1816 and",
  "1817 are both 0.50 mm, a duplicate. Delete the row for <b>1817</b> with",
  "<b>Fix Last Year unchecked</b>: this series was placed by its older part, so",
  "its first year is the one to keep. As always, only the wood tells you what",
  "really happened.")
