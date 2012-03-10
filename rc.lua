-- example code for combined kbdd (https://github.com/qnikst/kbdd) 
-- and emacs input method notification
kbdwidget = widget({type = "textbox", name = "kbdwidget"})
kbdwidget.text = " | En |"
kbd_text="En"
emacs_text=""
dbus.request_name("session", "ru.gentoo.kbdd")
dbus.add_match("session", "interface='ru.gentoo.kbdd',member='layoutChanged'")
dbus.add_signal("ru.gentoo.kbdd", function(...)
    local data = {...}
    local layout = data[2]
    local font = theme.kbd_font or theme.font or ""
    lts = {[0] = "En", [1] = "Ua", [2] = "Ru"}
    kbd_text= lts[layout]
    kbdwidget.text = " | "..emacs_text..kbd_text.." |"
    end
)

dbus.request_name("system", "org.gnu.Emacs")
dbus.add_match("system", "interface='org.gnu.Emacs',member='imChanged'")
dbus.add_signal("org.gnu.Emacs", function(...)
				    local data = {...}
				    local layout = data[2]
				    if "nil" == layout then
				       emacs_text=""
				    else
				       emacs_text="<span color='#f05e6a'>"..layout.."</span> "
				    end
				    kbdwidget.text = " | "..emacs_text..kbd_text.." |"
				 end
	     )

-- Notify emacs of focus change event
client.add_signal("focus", function(c) 
			      c.border_color = beautiful.border_focus 
			      if c.class == "Emacs" then
				 awful.util.spawn("dbus-send --system /org/awesome/im org.awesome.im.imRequest uint32:1")
			      else 
				 emacs_text=""
				 kbdwidget.text = " | "..emacs_text..kbd_text.." |"
			      end
			   end)
client.add_signal("unfocus", function(c) 
				c.border_color = beautiful.border_normal
				if c.class == "Emacs" then
				   emacs_text=""
				   kbdwidget.text = " | "..emacs_text..kbd_text.." |"
				end
			     end)

