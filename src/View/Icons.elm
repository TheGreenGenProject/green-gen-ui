module View.Icons exposing (..)


import Ant.Icon exposing (..)
import Ant.Icons as Icons



tiny = [width 12, height 12]

small = [width 16, height 16]

normal = [width 24, height 24]

large = [width 48, height 48]

extraLarge = [width 96, height 96]

spinning:  List (Attribute msg) ->  List (Attribute msg)
spinning attrs = attrs ++ [spin]


wall = Icons.homeOutlined

feed = Icons.globalOutlined

event = Icons.calendarOutlined

pinned = Icons.pushpinFilled

unpinned = Icons.pushpinOutlined

notifications = Icons.bellOutlined

tip = Icons.bulbFilled

challenge = Icons.riseOutlined

poll = Icons.pieChartOutlined

post = Icons.userOutlined

repost = Icons.sendOutlined

search = Icons.searchOutlined

like = Icons.heartFilled

unlike = Icons.heartOutlined

user = Icons.userOutlined

email = Icons.mailOutlined

password = Icons.lockOutlined

logoff = Icons.logoutOutlined

flagged = Icons.flagFilled

flag = Icons.flagOutlined

openConversation = Icons.messageOutlined

closeConversation = Icons.messageFilled

comment = Icons.messageOutlined

platformNotification = Icons.notificationFilled

postNotification = like

challengeNotification = challenge

pollNotification = challenge

eventNotification = event

newFollowerNotification = Icons.usergroupAddOutlined

back = Icons.arrowLeftOutlined

square = Icons.checkSquareFilled

calendar = Icons.calendarOutlined

report = Icons.pieChartOutlined

successMeasure = Icons.slidersOutlined

followers = Icons.usergroupAddOutlined

arrowUp = Icons.caretUpFilled

arrowDown = Icons.caretDownFilled

plus = Icons.plusCircleFilled

minus = Icons.minusCircleFilled

spinningWheel attrs = Icons.loadingOutlined (attrs ++ [spin])

valid = Icons.checkCircleOutlined

invalid = Icons.closeCircleOutlined

