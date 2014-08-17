        LFN-QBasic is a series of subroutines designed to allow a
QBasic program to access them there Long File Names available under
Win9x without needing to know what the DOS8.3 name looks like. They're
easy to use, and simple enough to where you can upgrade existing code
without any trouble.  Ok, maybe a little trouble because you do have to
go through your code and replace all your OPEN statements with a
function call. And if you decide to use LFN-QBasic's automatic FREEFILE
action, you have to adjust for that as well.

        Otherwise the package is public domain, I do ask that if you
find the program usefull, drop me a note, heck, send a donation! I'd
appreciate that!  In any case, feel free to contact me, I don't bite.

        I should point out that the package is not limited to QBasic
except by the fact that it's in that format, using no line numbers and
having function calls.  With a little work, it can be adapted to to ANY
basic implementation.

        Full details of the program are in the LFNBASIC.HTM and LFNBASIC.TXT

Colin Glenn
cwg01@gnofn.org
718 Central Ave. #205
Jefferson  LA  70121

07/18/99 Repaired Fatal Flaw:
	There was an error DOSSEARCH where it opened the file as 1