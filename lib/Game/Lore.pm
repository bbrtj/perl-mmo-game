package Game::Lore;

use Moo::Role;

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
	return _lt($self->lore_id, 'name');
}

sub lore_description ($self)
{
	return _lt($self->lore_id, 'description');
}

