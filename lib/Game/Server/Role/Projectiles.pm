package Game::Server::Role::Projectiles;

use My::Moose::Role;
use Game::Config;
use Game::Object::Projectile;
use Game::Mechanics::Generic;
use Game::Mechanics::Projectile;
use Game::RNG;
use Math::Trig qw(deg2rad);

use all 'X';
use all 'Resource';

use header;

requires qw(
	map
	location
	find_in_radius
);

has param '_projectiles' => (
	isa => HashRef [InstanceOf ['Game::Object::Projectile']],
	default => sub { {} },
);

sub _projectile_hit ($self, $projectile, $send)
{
	if ($send) {
		$self->send_to_players(
			$projectile->discovered_by,
			Resource::ProjectileStop->new(subject => $projectile),
		);
	}

	$self->apply_effect($projectile->effect, $projectile->xy);
	delete $self->_projectiles->{$projectile->id};
	return;
}

sub _process_projectiles ($self)
{
	my $map = $self->map;
	my $elapsed = server_time;

	foreach my $projectile (values $self->_projectiles->%*) {

		# a wall has been hit
		if (!Game::Mechanics::Projectile->travel($projectile, $map, $elapsed)) {
			$self->_projectile_hit($projectile, true);
			next;
		}

		# projectile ran out of range - no need to notify clients, since client
		# is aware of the max distance
		if ($projectile->finished) {
			$self->_projectile_hit($projectile, false);
			next;
		}

		# collision with actors
		# TODO: do not hit if target is friendly
		# NOTE: use radius 0, as other radius results in unnatural collisions
		my $actor = $projectile->actor;
		my @collision = grep { $_ != $actor }
			Game::Mechanics::Distance->find_actors_in_range($self, $projectile->xy, 0);

		$self->_projectile_hit($projectile, true)
			if @collision;
	}

	return;
}

sub spawn_projectile ($self, $actor, $lore, $effect, $at_x, $at_y)
{
	my $projectile_data = $lore->projectile;
	my ($angle) = Game::Mechanics::Generic->calculate_angle_and_diagonal($actor->variables->xy, $at_x, $at_y);

	if (my $inacc = $projectile_data->{inaccuracy} / 2) {
		my $roll = 1 - $actor->rng;
		my $side = rng() <=> 0.5;
		$angle += deg2rad $roll * $inacc * $side;
	}

	# TODO: check if actor is facing the right way
	# TODO: actual character radius
	my ($x, $y) = Game::Mechanics::Generic->find_frontal_point($actor->variables->xy, $angle, $actor->stats->size);

	my $projectile = Game::Object::Projectile->new(
		x => $x,
		y => $y,
		actor => $actor,
		effect => $effect,
		speed => $projectile_data->{speed},
		angle => $angle,
		max_distance => $projectile_data->{range},
	);

	# NOTE: data about the projectile is sent to all players who can ever see it (for all practical purposes)
	my @actors = Game::Mechanics::Distance->find_actors_in_range(
		$self, $x, $y,
		$projectile_data->{range} * 2 + Game::Config->discover_radius
	);
	$projectile->set_discovered_by([map { $_->id } @actors]);

	$self->_projectiles->{$projectile->id} = $projectile;

	$self->send_to_players(
		$projectile->discovered_by,
		Resource::Projectile->new(subject => $projectile),
	);

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(0.2 => '_process_projectiles', 8);
};

