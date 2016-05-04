CREATE TABLE IF NOT EXISTS family (
    family_name text PRIMARY KEY,
);

CREATE TABLE IF NOT EXISTS genus (
    genus_family text NOT NULL REFERENCES family (family_name),
    genus_name text PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS species (
    species_genus text NOT NULL REFERENCES genus (genus_name),
    species_name text PRIMARY KEY
);

INSERT INTO family VALUES
    ('Fagacea'),
    ('Malvacea');

INSERT INTO genus VALUES
    ('Fagacea', 'Castanea'),
    ('Malvacea', 'Tilia');

INSERT INTO species VALUES
    ('Tilia', 'Tilia europeae'),
    ('Tilia', 'Tilia tomentosa');

