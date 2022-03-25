lore INTERN => attribute 'Internal';

translations pl => {
	name => 'Wewnętrzne',
};

# PHYSICAL DAMAGE TYPES

lore PHYS => attribute 'Physical';

translations pl => {
	name => 'Fizyczne',
};

lore PUNC => attribute 'Punctured';
subtype_of attribute 'Physical';

translations pl => {
	name => 'Kłute',
};

lore CRUSH => attribute 'Crushed';
subtype_of attribute 'Physical';

translations pl => {
	name => 'Miażdżone',
};

lore SLASH => attribute 'Slashed';
subtype_of attribute 'Physical';

translations pl => {
	name => 'Cięte',
};

# MAGICAL DAMAGE TYPES

lore MAGIC => attribute 'Magical';

translations pl => {
	name => 'Magiczne',
};

lore MENTAL => attribute 'Mental';
subtype_of attribute 'Magical';

translations pl => {
	name => 'Umysłowe',
};

lore DARK => attribute 'Dark';
subtype_of attribute 'Magical';

translations pl => {
	name => 'Mroczne',
};

lore HOLY => attribute 'Holy';
subtype_of attribute 'Magical';

translations pl => {
	name => 'Święte',
};

# ELEMENTAL DAMAGE TYPES

lore ELEM => attribute 'Elemental';

translations pl => {
	name => 'Od żywiołów',
};

lore FIRE => attribute 'Fire';
subtype_of attribute 'Elemental';

translations pl => {
	name => 'Od ognia',
};

lore WATER => attribute 'Water';
subtype_of attribute 'Elemental';

translations pl => {
	name => 'Od wody',
};

lore EARTH => attribute 'Earth';
subtype_of attribute 'Elemental';

translations pl => {
	name => 'Od ziemi',
};

lore AIR => attribute 'Air';
subtype_of attribute 'Elemental';

translations pl => {
	name => 'Od powietrza',
};

