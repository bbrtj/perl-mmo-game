requires 'attributes';
requires 'slots';

lore DAGGER => item 'Dagger';

uses attribute 'Punctured';
uses slot 'Left hand';
uses slot 'Right hand';

type 'weapon';

translations pl => {
	name => 'Sztylet'
};

define {
	damage => [3, 5],
	both_hands => 0,
};

# Base accuracy

