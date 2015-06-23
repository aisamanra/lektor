# Lektor: A Standard for Feed Readers

**NOTE:** The entirety of this file is tentative, and subject to change
at any time.

There are two main tasks for a feed reader: _fetching_ and _viewing_.
These two tasks, in the `lektor` system, are split apart into different
components, mediated by a `lektordir` system. There are two main parts
to the `lektor` architecture: the `lektor-dir` format and the
`lektor-entry` format.

## `lektor-feed`

A given `feed` consists of at least a `name` (which is human-readable)
and an `id` which unambiguously identifies the `feed` (which is
a URI). Information about `feed`s is stored in the `src` directory
inside a `lektor-dir`. Information about a given feed is stored inside
`src/$hash`, where `$hash` is the SHA-1 hash of of the `feed`'s `id`.

Obligatory elements include:

- `id`: The URI which identifies the feed. In the case of
RSS/Atom/ActivityStream feeds, this will generally be the URL at
which the feed is hosted. For other things—for example, for
services which may not have a web equivalent—it might instead be
a tag URI or some other opaque identifier.
- `name`: The human-readable name of the feed. This is
produced by the fetcher and should not be changed by a viewer.

Optional elements include:

- `description`: A human-readable description describing
the feed.
- `language`: The language the feed is written in.
- `image`: An image that can be optionally displayed with
the channel.
- `copyright`: The copyright notice for the feed.
- `author`: Authorship information for the feed.

### Feed example

A minimal feed might look like

```.bash
cd $LEKTORDIR
HASH=$(printf 'http://example.com/rss.xml' | sha1sum)
mkdir -p $HASH

echo http://example.com/rss.xml  >$HASH/id
echo Example Feed                >$HASH/name
```

A feed with more entries might look like

```.bash
cd $LEKTORDIR
HASH=$(printf 'http://example.com/rss.xml' | sha1sum)
mkdir -p $HASH

echo http://example.com/rss.xml           >$HASH/id
echo Example Feed                         >$HASH/name
echo 'An example feed.'                   >$HASH/description
echo en-us                                >$HASH/language
echo http://example.com/image.png         >$HASH/image
echo Copyright 2015, Getty Ritter         >$HASH/copyright
echo 'Getty Ritter <gdritter@gmail.com>'  >$HASH/author
```

## `lektor-entry`

In contrast to `maildir`, entries in a `lektor-dir` are not files
but directories adhering to a particular structure.

Obligatory elements include:

- `title`: The title of the entry.
- `id`: The URI which identifies the entry. This will often be a
URL at which the resource corresponding to the entry is available,
but may also be an opaque identifier.
- `content`: TBD
- `feed`: A directory that contains all the information about the
source `feed`. This will generally be a symlink 

Optional elements include:

- `author`: Names and email addressess of the authors of the entry.
- `pubdate`: When the entry was published.

## `lektor-dir`

A `lektordir` is a directory with at least four subdirectories: `tmp`,
`new`, `cur`, and `src`. A _fetcher_ is responsible for examining a feed
and adding new entries the `lektordir` according to the following process:

- The fetcher `chdir()`s to the `lektordir` directory.
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
- The fetcher `link()`s the file to `new/$feed/$time.$pid.$host`. At that
instant, the entry has been successfully created.

A _viewer_ is responsible for displaying new feed entries to a user
through some mechanism. A viewer looks through the `new` directory for
new entries. If there is a new entry, `new/$feed/$unique`, the viewer may:

- Display the contents of `new/$feed/$unique`
- Delete `new/$feed/$unique`
- Rename `new/$feed/$unique`.

A `lektordir` can contain arbitrary other directories, but for the sake
of compatibility, these should attempt to adhere to the following
schema:

- If the extra directory contains configuration or other information
for a given feed, it 
