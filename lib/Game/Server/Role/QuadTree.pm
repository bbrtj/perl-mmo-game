package Game::Server::Role::QuadTree;

use My::Moose::Role;
use Game::Config;
use POSIX qw(ceil);

use header;

requires qw(
	location_data
);

sub find_in_radius;
has field '_quad_tree' => (
	isa => Types::InstanceOf ['Algorithm::QuadTree'],
	lazy => 1,
	'handles->' => {
		'find_in_radius' => 'getEnclosedObjects',
	},
);

sub _build_quad_tree ($self)
{
	my $location = $self->location_data->location;
	croak 'no map for location ' . $location->id
		unless $location->data->has_map;

	my $map = $location->data->map;

	my $size = (sort { $b <=> $a } ($map->size_x, $map->size_y))[0];
	my $required_precision = $size / Game::Config->config->{base_radius};
	my $required_depth = ceil(log($required_precision) / log(2));

	return Algorithm::QuadTree->new(
		-depth => $required_depth,
		-xmin => 0,
		-ymin => 0,
		-xmax => $map->size_x,
		-ymax => $map->size_y,
	);
}

sub _reload_coordinates ($self, $elapsed_time)
{
	my $qt = $self->_quad_tree;
	$qt->clear;

	state $radius = Game::Config->config->{base_radius};
	foreach my $actor ($self->location_data->actors->@*) {
		my $variables = $actor->variables;
		$qt->add($actor, $variables->pos_x, $variables->pos_y, $radius);
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_reload_coordinates');
};

