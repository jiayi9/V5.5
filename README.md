# deepdive
For seagate users only. This app is used for ad-hoc analysis for finding root failure causes at both drive and head levels.

Make sure you have these two columns: "DRIVE.SERIAL.NUM"(or "DRIVE_SERIAL_NUM", case-insensitive) and "STATUS" (the target variable in "F"s and "P"s).

You can run this app by entering the following code in R studio console. Required packages will be automatically installed if they are not installed in your local machine.

    shiny::runGitHub("deepdive","jiayi9")
