package Game::LoreLoader::LoreDummy;

use My::Moose;
use Types;
use Game::Lore;

use header;

has 'name' => (
	is => 'ro',
	isa => Types::Str,
	required => 1,
);

has 'class' => (
	is => 'ro',
	isa => Types::Str,
	required => 1,
);

sub create ($self)
{
	my $stored = Game::Lore->get_named($self->class, $self->name);

	return $stored;
}

