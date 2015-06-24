#/bin/bash -e

cd $LEKTORDIR

for FEED in $(ls new)
do
    mkdir -p cur/$FEED

    # print feed header
    echo "In feed $(cat src/$FEED/name):"
    echo

    for ENTRY in $(ls new/$FEED)
    do
        # print entry
        echo "$(cat new/$FEED/$ENTRY/title)"
        cat new/$FEED/$ENTRY/content | head -n 4
        echo

        # move entry to `cur`
        mv new/$FEED/$ENTRY cur/$FEED/$ENTRY
    done
done
