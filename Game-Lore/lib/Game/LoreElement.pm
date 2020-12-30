package Game::LoreElement;

use header;
use Moo::Role;
use Game::Common::Container;

no header;

requires qw(
	lore_id
	_get
);

sub get ($self, $id = undef)
{
	return $self->_get->{defined $id ? $id : $self->lore_id};
}

sub lore_name ($self)
{
	return resolve('repo')->lore_data->load(name => $self->lore_id);
}

sub lore_description ($self)
{
	return resolve('repo')->lore_data->load(description => $self->lore_id);
}

1;
