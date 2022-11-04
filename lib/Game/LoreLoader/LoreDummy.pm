package Game::LoreLoader::LoreDummy;

use My::Moose;
use Game::Lore;

use header;

has param 'name' => (
	isa => Types::Str,
);

has param 'class' => (
	isa => Types::Str,
);

sub create ($self)
{
	state $repo = DI->get('lore_data');
	my $stored = $repo->load_named($self->class, $self->name);

	return $stored;
}

