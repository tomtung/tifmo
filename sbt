#!/bin/sh
SBT_OPTS="-Xms512M -Xmx1536M -Xss1M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=256M"
test -f ~/.sbtconfig && . ~/.sbtconfig
exec java ${SBT_OPTS} -jar sbt-launch.jar "$@"

