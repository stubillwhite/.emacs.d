# org-mode protocol bookmarks #

## org-mode link ##

javascript:location.href='org-protocol://store-link?url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)

## org-mode capture ##

javascript:location.href='org-protocol://capture://'+encodeURIComponent(location.href)+'/'+encodeURIComponent(document.title)+'/'+encodeURIComponent(window.getSelection())

## org-mode rtc ##

javascript:location.href='org-protocol://capture://r/'+encodeURIComponent(location.href)+'/'+encodeURIComponent(document.title)+'/'+encodeURIComponent(window.getSelection())
