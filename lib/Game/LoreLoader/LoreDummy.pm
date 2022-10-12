package Game::LoreLoader::LoreDummy;

use My::Moose;
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
	state $repo = DI->get('lore_data');
	my $stored = $repo->load_named($self->class, $self->name);

	return $stored;
}

