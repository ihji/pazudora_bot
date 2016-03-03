#!/bin/sh
java -Xms512M -Xmx1536M -Xss8M -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=384M -jar `dirname $0`/sbt-launch.jar "$@"
