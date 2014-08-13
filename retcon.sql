--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | The `retcon` table is the primary store if a particular entity document in
-- the database.
CREATE TABLE retcon (
    entity VARCHAR(64) NOT NULL,
    id SERIAL NOT NULL,

    PRIMARY KEY (entity, id)
);

-- | The `retcon_fk` table records information about a particular entity
-- document as stored in a foreign system.
CREATE TABLE retcon_fk (
    entity VARCHAR(64) NOT NULL,
    id     INTEGER NOT NULL,
    source VARCHAR(64) NOT NULL,
    fk     TEXT NOT NULL,

    PRIMARY KEY (entity, source, fk),
    UNIQUE (entity, id, source),
    FOREIGN KEY (entity, id) REFERENCES retcon (entity, id)
);

-- | The `retcon_initial` table records the "initial" document used in the last
-- run for a particular entity document.
CREATE TABLE retcon_initial (
    entity VARCHAR(64) NOT NULL,
    id     INTEGER NOT NULL,
    document JSON NOT NULL,

    PRIMARY KEY (entity, id),
    FOREIGN KEY (entity, id) REFERENCES retcon (entity, id)
);

