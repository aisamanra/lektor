# Lektor: A Standard for Feed Readers

**NOTE:** The entirety of this file is tentative, and subject to change
at any time.

There are two main tasks for a feed reader: _fetching_ and _viewing_.
These two tasks, in the `lektor` system, are split apart into different
components, mediated by a `lektor-dir` system. A `lektor-dir` contains
two kinds of information: information about feeds (sources of new
entries) and information about entries themselves.

# At A Glance

A given user has their own `lektor-dir`. A `lektor-dir` contains both
"feeds" and "entries". Two kinds of programs operate on `lektordir`s
in two different capcities: a _fetcher_ produces entries for one or
more feeds, and a _viewer_ manages entries once produced and shows
them to some user. A given `lektor-dir` can have multiple fetchers
and multiple viewers operating on it.

The rationale for these decisions is this:

- Separating fetchers from viewers means that a user can easily
  mix-and-match different front-ends and back-ends.
- Allowing multiple fetchers allows different entry sources to be
  handled independently, ideally allowing those programs to be
  simpler.
- Allowing multiple viewers means that a user can track multiple
  feeds but view the information from those feeds in ways which
  are more or less appropriate.
- Keeping this information split apart in the file system, rather
  than in a database or text file, both improves the ability to
  operate concurrently on different parts of a `lektor-dir` and
  lifts the burden of parsing information from the implementer.
  The file system is generally used here as a kind of hierarchical
  key-value store.
- The overall design is lifted straight from the `maildir` format,
  which is a time-tested and well-understood format for email. This
  modifies it slightly and adds a richer structure for RSS-like
  applications.

## `lektor-feed`

A given `feed` consists of at least a `name` (which is human-readable)
and an `id` which unambiguously identifies the `feed` (which is
a URI). Information about `feed`s is stored in the `src` directory
inside a `lektor-dir`. Information about a given feed is stored inside
`src/$hash`, where `$hash` is the SHA-1 hash of of the `feed`'s `id`.

Obligatory elements for a `feed` include:

- `id`: The URI which identifies the feed. In the case of
RSS/Atom/ActivityStream feeds, this will generally be the URL at
which the feed is hosted. For other things—for example, for
services which may not have a web equivalent—it might instead be
a tag URI or some other opaque identifier.
- `name`: The human-readable name of the feed. This is
produced by the fetcher and should not be changed by a viewer.

Optional elements for a `feed` include:

- `description`: A human-readable description describing the feed.
- `language`: The language the feed is written in.
- `image`: An image that can be optionally displayed with the channel.
- `copyright`: The copyright notice for the feed.
- `author`: Authorship information for the feed.

### Feed example

A minimal feed might look like

~~~{.bash}
# $HASH is sha1sum('http://example.com/rss.xml')
HASH=80af8e84e5ef7ae6b68acb8d1987e58e3e5731dd
cd $HASH

echo 'http://example.com/rss.xml'  >id
echo 'Example Feed'                >name
~~~

A feed with more entries might look like

~~~{.bash}
# $HASH is sha1sum('http://example.com/rss.xml')
HASH=80af8e84e5ef7ae6b68acb8d1987e58e3e5731dd
cd $HASH

echo 'http://example.com/rss.xml'         >id
echo 'Example Feed'                       >name
echo 'An example feed.'                   >description
echo 'en-us'                              >language
echo 'http://example.com/image.png'       >image
echo 'Copyright 2015, Getty Ritter'       >copyright
echo 'Getty Ritter <gdritter@gmail.com>'  >author
~~~

## `lektor-entry`

In contrast to `maildir`, entries in a `lektor-dir` are not files
but directories adhering to a particular structure.

Obligatory elements for an `entry` include:

- `title`: The title of the entry.
- `id`: The URI which identifies the entry. This will often be a
URL at which the resource corresponding to the entry is available,
but may also be an opaque identifier.
- `content`: **TBD**
- `feed`: A directory that contains all the information about the
source `feed`. This will generally be a soft link to the relevant
`feed` directory, but programs should not assume that it is.

Optional elements for an `entry` include:

- `author`: Names and email addressess of the authors of the entry.
- `pubdate`: When the entry was published.
- `type`: The MIME type of the content. If `type` is not present,
the assumed content type is `text/plain`.

### Entry example

A minimal entry might look like

~~~{.bash}
# $FEED is sha1sum('http://example.com/rss.xml')
FEED=80af8e84e5ef7ae6b68acb8d1987e58e3e5731dd
echo 'Example Entry'               >title
echo 'http://example.com/example'  >id
echo 'A sample entry.'             >content
ln -s $LEKTOR-DIR/src/$FEED          feed
~~~

A full entry might look like

~~~{.bash}
# $FEED is sha1sum('http://example.com/rss.xml')
FEED=80af8e84e5ef7ae6b68acb8d1987e58e3e5731dd
echo 'Example Entry'                         >title
echo 'http://example.com/example'            >id
echo 'A sample entry.'                       >content
echo 'Getty Ritter <gettyritter@gmail.com>'  >author
echo '2015-06-23T13:06:22Z'                  >pubdate
echo 'text/html'                             >type
ln -s $LEKTOR-DIR/src/$FEED                    feed
~~~

## `lektor-dir`

A `lektor-dir` is a directory with at least four subdirectories: `tmp`,
`new`, `cur`, and `src`. A _fetcher_ is responsible for examining a feed
and adding new entries the `lektor-dir` according to the following process:

- The fetcher `chdir()`s to the `lektor-dir` directory.
- The fetcher `stat()`s the name `tmp/$feed/$time.$pid.$host`, where
`$feed` is the hash of the feed's `id` value, `$time`
is the number of seconds since the beginning of 1970 GMT, `$pid` is the
program's process ID, and `$host` is its host name.
- If `stat()` returned anything other than `ENOENT`, the program sleeps
for two seconds, updates `$time`, and tries the `stat()` again, a limited
number of times.
- The fetcher creates the directory `tmp/$feed/$time.$pid.$host`.
- The fetcher writes the entry contents (according to the `lektor-entry`
format) to the directory.
- The fetcher moves the file to `new/$feed/$time.$pid.$host`. At that
instant, the entry has been successfully created.

A _viewer_ is responsible for displaying new feed entries to a user
through some mechanism. A viewer looks through the `new` directory for
new entries. If there is a new entry, `new/$feed/$unique`, the viewer may:

- Display the contents of `new/$feed/$unique`.
- Delete `new/$feed/$unique`.
- Rename `new/$feed/$unique` to `cur/$feed/$unique;$info`.

A `lektor-dir` can contain other information not specified here, but that
information should attempt to adhere to these guidelines:

- If the extra information pertains to a particular feed, it should appear
in the directory `src/$feed/etc`
- If the extra information pertains to a fetcher, it should appear in the
directory `etc/fetch`.
- If the extra information pertains to a viewer, it should appear in the
directory `etc/view`.

## Possibilities for `lektor`

Lektor lends itself well to web syndication (e.g. RSS, Atom,
ActivityStreams, &c) but could be used for any kind of stream of
information. For example, a fetcher might serve as a mediated logging
service for other information such as regular load information on a
running web service, pushing updates into a shared `lektor-dir` on a
regular basis. It would also be trivial to write custom fetchers for
services that no longer expose RSS or other syndication formats, such
as Twitter.

Here is a trivial fetcher that provides a feed of timestamps every
hour:

~~~{.bash}
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
~~~

Additionally, multiple viewers can act on the same `lektor-dir`. A
given viewer need not show every piece of information: for example,
a viewer may sniff the `type` attribute of entries and only display
entries of a given type, or selectively choose which feeds to display,
or even select entries at random to display. It also has full control
over how to display those entries.

Here is a trivial viewer that shows a small digest of each entry in
`new` and then moves those entries to `cur`:

~~~{.bash}
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
~~~
