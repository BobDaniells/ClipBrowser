"ClipBrowser" is a Linux or Windows desktop application that gathers all of 
 your videos into one library to facilitates rapid search and play operations.
 As such it has similar functionality to the Linux "gmusicbrowser" browser 
 that targets music CDs. It can handle local video files and/or YouTube links.

 Features:

 1. Video files are stored in a database as file paths (or URL's in the case of
    YouTube clips. Local files can be added to the database individually, or as
    groups, or as dragging and dropping onto the program's main window. There
    is also a menu option to find and load all video files under a specified 
    root path. You add YoutTube links by copying and pasting URLs from a web
    browser.

 2. Library content is displayed as a matrix of thumbnails together with
    titles. For local files these thumbnails are auto-generated, or downloaded
    in the case of YouTube videos. Thumbnails are stored in the default
    thumbnail path for the relevant OS. 

 3. Any video can be instantly selected simply by clicking its thumbnail and/or
    played by double-clicking its thumbnail. ClipBrowser provides a choice of
    (separately=installed) players like VLC for local video files. The default
    web browser (with an active Internet connection) is used for YouTube videos.

 4. The displayed thumbs may be filtered by:

        a. A text string that represents any substring of the title.
           E.g. Entering 'Bach' would show only videos with 'Bach' in the title.

        b. Up to 32 keywords that get stored in the library along with the
           (This is far more convenient than the alternative of manually finding
           and playing videos that may be stored in multiple OS directories).

        c. A category like 'Defaut', 'Music', 'How To', 'Pets', etc.
           videos are visible).

  5. The thumb matrix uses a 'lazy loading' strategy to optimize load times and
    responsiveness.  I.e., when the app is opened only one page of thumbs
    (typically 15) is loaded. Scrolling the display down by one row then loads
    the next row of thumbs (typically 5), and so on. This thumb matrix is fully
    responsive in terms of displaying more or fewer thumbs on resizing the
    window.

 7. Each thumb can also any number of have document files attached. For example,
    if a particular thumb contains a music video then it is possible to add
    associated sheet music as a PDF file. Double clicking any item in the document
    list for the thumb opens the associated file with the default application
    for that file type.

 8. On first run the database and thumb matrix will be empty, but a menu option
    loads a range of test data from YouTube. Note that this action is very fast
    as it does not download and store any files, but rather just the URI's of
    those files. This also avoids potential copyright implications.

 9. ClipBrowser also contains parental control options.

 10. ClipBrowser showcases the capabilities of the Synopse mORMot object-
    oriented database framework with Lazarus. The libary database is
    automatically created at program startup. Data access is extremely fast.

 Copyright Bob Daniells, 10/09/2022.
 License: GPL

 Compile with Lazarus 2.1.12 or later and Synopse mORMot 1.18.
 Other dependencies include ffmpeg, BGRABitmap, BGRAControls,
 ffmpeg, plus your choice of video players.
 