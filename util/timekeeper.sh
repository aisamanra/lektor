#!/bin/bash -e

cd $LEKTORDIR

# the feed information
ID='tag:example.com:timekeeper'
HASH=$(printf $ID | sha1sum | awk '{ print $1; }' )

# other metadata
HOST=$(hostname)
MAX=10

# create the feed
mkdir -p src/$HASH
echo $ID         >src/$HASH/id
echo Timekeeper  >src/$HASH/name

mkdir -p "tmp/$HASH"
mkdir -p "new/$HASH"

# create entries every hour
while true; do
    TIME=$(date '+%s')
    ENTRY="$HASH/$TIME.$$.$HOST"

    # if the file exists, wait two seconds and try again
    RETRY=0
    while [ -e $ENTRY ]
    do
        # if we've waited more than $MAX times, then
        # give up
        if [ $RETRY -gt $MAX ]; then
            exit 1
        fi
        sleep 2
        RETRY=$(expr $RETRY + 1)
    done

    # create the entry
    mkdir -p tmp/$ENTRY

    # create entry values
    echo 'Current Time'                      >tmp/$ENTRY/title
    echo $TIME                               >tmp/$ENTRY/content
    echo "tag:example.com:timekeeper#$TIME"  >tmp/$ENTRY/id
    ln -s $LEKTORDIR/src/$HASH                tmp/$ENTRY/feed

    # move the entry to the new location
    mv tmp/$ENTRY new/$ENTRY

    # wait for half an hour and do it again
    sleep 3600
done
