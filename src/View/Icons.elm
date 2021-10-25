module View.Icons exposing (..)


import Ant.Icon exposing (..)
import Ant.Icons as Icons


toWidthHeight: (Int, Int) -> List (Attribute msg)
toWidthHeight (w, h) = [width w, height h]

spinning:  List (Attribute msg) ->  List (Attribute msg)
spinning attrs = attrs ++ [spin]

wall = toWidthHeight >> Icons.homeOutlined

feed = toWidthHeight >> Icons.globalOutlined

event = toWidthHeight >> Icons.calendarOutlined

pinned = toWidthHeight >> Icons.pushpinFilled

unpinned = toWidthHeight >> Icons.pushpinOutlined

notifications = toWidthHeight >> Icons.bellOutlined

tip = toWidthHeight >> Icons.bulbFilled

challenge = toWidthHeight >> Icons.riseOutlined

poll = toWidthHeight >> Icons.pieChartOutlined

post = toWidthHeight >> Icons.userOutlined

repost = toWidthHeight >> Icons.shareAltOutlined

search = toWidthHeight >> Icons.searchOutlined

like = toWidthHeight >> Icons.heartFilled

unlike = toWidthHeight >> Icons.heartOutlined

user = toWidthHeight >> Icons.userOutlined

email = toWidthHeight >> Icons.mailOutlined

password = toWidthHeight >> Icons.lockOutlined

logoff = toWidthHeight >> Icons.logoutOutlined

flagged = toWidthHeight >> Icons.flagFilled

flag = toWidthHeight >> Icons.flagOutlined

openConversation = toWidthHeight >> Icons.messageOutlined

closeConversation = toWidthHeight >> Icons.messageFilled

comment = toWidthHeight >> Icons.messageOutlined

platformNotification = toWidthHeight >> Icons.notificationFilled

postNotification = like

challengeNotification = challenge

pollNotification = challenge

eventNotification = event

newFollowerNotification = toWidthHeight >> Icons.usergroupAddOutlined

back = toWidthHeight >> Icons.arrowLeftOutlined

square = toWidthHeight >> Icons.checkSquareFilled

calendar = toWidthHeight >> Icons.calendarOutlined

report = toWidthHeight >> Icons.pieChartOutlined

successMeasure = toWidthHeight >> Icons.slidersOutlined

followers = toWidthHeight >> Icons.usergroupAddOutlined

arrowUp = toWidthHeight >> Icons.caretUpFilled

arrowDown = toWidthHeight >> Icons.caretDownFilled

plus = toWidthHeight >> Icons.plusCircleFilled

minus = toWidthHeight >> Icons.minusCircleFilled

spinningWheel (w,h) = Icons.loadingOutlined (toWidthHeight (w,h) ++ [spin])

valid = toWidthHeight >> Icons.checkCircleOutlined

invalid = toWidthHeight >> Icons.closeCircleOutlined

location = toWidthHeight >> Icons.environmentOutlined

loadMore = toWidthHeight >> Icons.redoOutlined

