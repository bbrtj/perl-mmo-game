package Tiled::ObjectList;

use My::Moose;

use header;

use constant TYPE_SPAWNS => 'spawns';
use constant TYPE_TO_LORE => {
	spawns => 'Game::Lore::Npc',
};

has param 'map' => (
	isa => InstanceOf ['Tiled::Map'],
	weak_ref => 1,
);

has field 'objects' => (
	isa => HashRef [ArrayRef [HashRef]],
	default => sub { {} },
	'handles{}' => {
		'get_objects_of_type' => 'get',
	},
);

sub add_object ($self, $type, $attributes, $properties = {})
{
	state $repo = DI->get('lore_data_repo');

	$type = lc $type;
	my $type_lore = TYPE_TO_LORE->{$type};

	die "unknown map object lore type $type"
		unless defined $type_lore;

	# NOTE: tiled marks height from the top
	my %object = (
		name => $attributes->{name},
		lore => $repo->load_named($type_lore, $attributes->{type}),
		x => $attributes->{x} / $self->map->tilewidth,
		y => $self->map->height - $attributes->{y} / $self->map->tileheight,
		%{$properties},
	);

	push $self->objects->{$type}->@*, \%object;
	return;
}

