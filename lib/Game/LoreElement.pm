package Game::LoreElement;

use Moo::Role;
use DI;

use header;

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
	return DI->get('repo')->lore_data->load(name => $self->lore_id);
}

sub lore_description ($self)
{
	return DI->get('repo')->lore_data->load(description => $self->lore_id);
}

