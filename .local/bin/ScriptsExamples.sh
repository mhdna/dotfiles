#/bin/sh


# Download a list of youtube videos in a website - using curl and grep
# i for ignore case, E for regex, o for only getting the greped thing instead of the entire line
# .{11} = ........... (11 any char)
curl -s https:website/music.html | grep -iEo "https:\/\/youtu\.be.{11}"
# then
wget $(curl -s https://website/music.html | grep -iEo "https:\/\/youtu\.be.{11}")

# Downloading all the images - using wget
wget -nd -r -A jpeg,jpg,bmp,gif,png https://website.org
