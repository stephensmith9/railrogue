created the railrogue file from a youtube presentation - see the top of the .el file.

put under git entirely by using emacs VC - first you make the buffer the current one, then go to Tools then Register, then tab to find
'Git' (or if you remember, just type in 'Git' - I tried git first and it said unknown module or something like that so I hit TAB for 
auto-complete and got a list of valid VC systems. Simple, it just asks for your repo folder (which defaults to the current one the buffer
is open in).

setup C-x g as the first keybinding (runs magit-status which we'll use all the time)
- note, I put this in the wrong init.el file first, but caught this later and put it in the right one (in AppData/Roaming/etc)
- you can repeat the C-x g to refresh the buffer (or even better just type 'g' while in any magit buffer)
- almost all magit commands can be run while in this buffer
- s to stage, u to unstage, n and p to move between sections, tab expands and collapses sections, c commits
- to commit type c, then c again to confirm then fill in the commit msg then type C-c C-c to do the actual commit

