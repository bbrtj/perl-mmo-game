package Game::Object::Actor::Npc::Ai::Wander;

use My::Moose;
use Game::RNG qw(rng random_number);
use Game::Mechanics::Generic;
use Math::Trig qw(pi);

use header;

extends 'Game::Object::Actor::Npc::Ai';

has param 'parent' => (
	lax_isa => InstanceOf ['Game::Object::Actor::Npc'],
	weak_ref => 1,
);

has param 'every_min' => (
	lax_isa => PositiveNum,
	default => 2,
);

has param 'every_max' => (
	lax_isa => PositiveNum,
	default => 4,
);

has param 'max_distance' => (
	lax_isa => PositiveOrZeroNum,
	default => 1,
);

has field 'last_wander' => (
	lax_isa => Num,
	writer => 1,
	default => sub { time },
);

sub act ($self, $server, $actor, $elapsed = server_time)
{
	return unless $elapsed >= $self->last_wander;

	my $angle = rng() * 2 * pi;
	my $distance = rng() * $self->max_distance;
	my @point = Game::Mechanics::Generic->find_frontal_point($self->parent->spawn->xy, $angle, $distance);
	$self->set_last_wander($elapsed + random_number $self->every_min, $self->every_max);

	$server->set_movement($actor->id, @point);
	return;
}

