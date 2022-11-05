package Game::Server;

use My::Moose;
use Algorithm::QuadTree;
use Game::Config;

use header;

has param 'process' => (
	isa => Types::InstanceOf['Server::Process::Game'],
	weak_ref => 1,
	'handles->' => {
		'send_to' => 'send_to',
		'log' => 'log',
	},
);

has param 'location_data' => (
	isa => Types::InstanceOf ['Unit::Location'],
);

has field '_quad_tree' => (
	isa => Types::InstanceOf ['Algorithm::QuadTree'],
	builder => 1,
	'handles->' => {
		'find_in_radius' => 'getEnclosedObjects',
	},
);

sub _build_quad_tree ($self)
{
	my $location = $self->location_data->location;
	croak 'no map for location ' . $location->id
		unless $location->data->has_map;

	return Algorithm::QuadTree->new(
		-depth => Game::Config->config->{quadtree_depth},
		-xmin => 0,
		-ymin => 0,
		-xmax => $location->data->map->size_x,
		-ymax => $location->data->map->size_y,
	);
}

sub reload_coordinates ($self)
{
	my $qt = $self->_quad_tree;
	$qt->clear;

	my $radius = Game::Config->config->{base_radius};
	for my $actor ($self->location_data->actors->@*) {
		my $variables = $actor->variables;
		$qt->add($actor, $variables->pos_x, $variables->pos_y, $radius);
	}

	return;
}

sub tick ($self, $elapsed)
{
	# TODO: process moves
	$self->reload_coordinates;
}

