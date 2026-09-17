-- "BCTU Dashboard.app": runs start_mac.sh from the folder the app sits in.
-- Built by build_mac_app.sh. The Desktop item is a Finder alias to this app,
-- so it keeps working if the project folder is renamed or moved; the path
-- below is the fallback for a copy of the app made somewhere else.
on run
	set appFolder to do shell script "dirname " & quoted form of (POSIX path of (path to me))
	set starter to appFolder & "/start_mac.sh"
	try
		do shell script "test -x " & quoted form of starter
	on error
		set starter to "__DESKTOP_DIR__/start_mac.sh"
	end try
	do shell script quoted form of starter & " > /dev/null 2>&1 &"
end run
