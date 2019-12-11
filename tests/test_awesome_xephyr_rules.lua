if is_screen_available(1) then
    table.insert(
        awful.rules.rules,
        { rule = { class = "Xephyr",
                   name  = "Xephyr on :1.0 (ctrl+shift grabs mouse and keyboard)" },
          properties = { floating   = true,
                         fullscreen = true,
                         screen     = 1} })
end
