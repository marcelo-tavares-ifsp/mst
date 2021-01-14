os.execute("unclutter &")
os.execute("sleep 2; "
           .. " sudo pkill mst-start-dm; "
           .. " sudo /bin/mst-start-dm " .. screen.count() .. " &")
