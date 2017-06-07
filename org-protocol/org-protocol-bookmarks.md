# org-mode protocol bookmarks #

## org-mode link ##

javascript:location.href='org-protocol://store-link?url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)

## org-mode capture ##

javascript:location.href='org-protocol://capture?url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&selection='+encodeURIComponent(window.getSelection())

## org-mode jira ##

javascript:location.href='org-protocol://capture?template=j&url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title)+'&selection='+encodeURIComponent(window.getSelection())
