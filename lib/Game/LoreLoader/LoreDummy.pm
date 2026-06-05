package Game::LoreLoader::LoreDummy;

use My::Moose;
use Game::Lore;

use header;

has param 'name' => (
	isa => Str,
);

has param 'class' => (
	isa => Str,
);

sub create ($self)
{
	state $repo = DI->get('lore_data_repo');
	my $stored = $repo->load_named($self->class, $self->name);

	return $stored;
}

