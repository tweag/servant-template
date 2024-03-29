CREATE TABLE users
( id UUID PRIMARY KEY
, name TEXT NOT NULL UNIQUE
, password TEXT NOT NULL
);

CREATE TABLE contents
( id UUID PRIMARY KEY
, content TEXT NOT NULL
, user_id UUID REFERENCES users (id) ON DELETE CASCADE ON UPDATE CASCADE
);

CREATE TABLE tags
( id UUID PRIMARY KEY
, name TEXT NOT NULL UNIQUE
);

CREATE TABLE contents_tags
( content_id UUID REFERENCES contents (id) ON DELETE CASCADE ON UPDATE CASCADE
, tag_id     UUID REFERENCES tags     (id) ON DELETE CASCADE ON UPDATE CASCADE
);
