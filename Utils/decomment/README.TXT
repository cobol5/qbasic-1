/*******************************************************************/
*********************************************************************
*                                                                   *
*                 F I L E   D E C O M M E N T E R                   *
*                        version 0.1.1a                             *
*                    Oracle (webmaster@qbnz.com)                    *
*                                                                   *
*                          R E A D M E                              *
*                                                                   *
*********************************************************************
*                                                                   *
*       -- Contents --                                              *
*                                                                   *
*  1.0  Introduction                                                *
*  - 1.1  Legal Notice                                              *
*  - 1.2  Version Info                                              *
*  - 1.2  What is File Decommenter?                                 *
*  - 1.3  Why would I need File Decommenter?                        *
*                                                                   *
*  2.0  Using File Decommenter                                      *
*  - 2.1 Quick instructions                                         *
*  - 2.2 Adapting File Decommenter for your own use                 *
*                                                                   *
*  3.0  Miscellaneous                                               *
*  - 3.1 Credits                                                    *
*  - 3.2 Files in the download                                      *
*                                                                   *
*********************************************************************
*                                                                   *
*                                                                   *
*  Introduction                                                     *
*  ------------                                                     *
*                                                                   *
*  ** Legal notice **                                               *
*  This program is ALPHA. So it may (somehow, inconcievibly) damage *
*  your stuff. This program is provided AS IS, without any          *
*  guarantee of fitness for any purpose whatsoever.This program is  *
*  released under the GPL.                                          *
*                                                                   *
*  ** Version Info **                                               *
*  0.1.1a - Fixed a small metacommand bug                           *
*  0.1.0a - Initial Release                                         *
*                                                                   *
*  ** What is File Decommenter? **                                  *
*                                                                   *
*  File Decommenter is a program written in QuickBASIC 4.5 that,    *
*  when run, will remove commenting from a Q(uick)basic program. It *
*  will remove apostrophe (') and REM style comments, but leave     *
*  metacommands (like '$INCLUDE: and '$DYNAMIC) alone. It also      *
*  removes blank lines, and to a certain extent, indenting.         *
*                                                                   *
*  ** Why would I need File Decommenter? **                         *
*  Simply, it saves space, should you ever need it ;). It doesn't   *
*  help with the 64K compiling limit, as tests have shown that      *
*  whitespace and commenting are ignored in the compiling process.  *
*  You may find a good use for it, if you do please contact me.     *
*                                                                   *
*  Using File Decommenter                                           *
*  ----------------------                                           *
*                                                                   *
*  ** Quick instructions **                                         *
*  Start QBasic and load comment.bas. Change the path$ variable to  *
*  point to where you want, either relative to the directory where  *
*  QBasic is, or an absolute path. Then simply run. Specify the     *
*  file that you would like the comments to be removed from, and    *
*  the name of the new file, and it will automatically run for you. *
*                                                                   *
*  ** Adapting File Decommenter for your own use **                 *
*  You can include the decomment function in your own program, and  *
*  simply pass the variables you need into it, and it will work     *
*  automatically, unless, of course, you specify a file that does   *
*  not exist, or do something else silly.                           *
*                                                                   *
*  Miscellaneous                                                    *
*  -------------                                                    *
*                                                                   *
*  ** Credits **                                                    *
*  Main programmer, tester   Oracle              webmaster@qbnz.com *
*  Compiling tester          Sterling Christensen                   *
*                                                                   *
*  ** Files in the download **                                      *
*  comment.bas          The main program                            *
*  test.bas             A test program for you to try decommenting  *
*  readme.txt           The file you're reading                     *
*                                                                   *
*********************************************************************
*                                                                   *
*  QBasic New Zealand - The home of QBasic Down Under (and the home *
*                       of the Oracle). Please take the time to     *
*                       visit! http://qbnz.com/                     *
*                                                                   *
*********************************************************************
/*******************************************************************/