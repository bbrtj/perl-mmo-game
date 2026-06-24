package Game::Server::Role::Discovery;

use My::Moose::Role;
use Game::Config;
use Resource::Discovery;

use header;

requires qw(
	location
	find_in_radius
	send_to_player
);

has cached '_discovered' => (
	isa => HashRef [HashRef [ULID]],
	default => sub { {} },
);

has cached '_discovered_by' => (
	writer => 1,
	isa => HashRef [ArrayRef [ULID]],
	default => sub { {} },
);

sub get_discovered_by ($self, $key)
{
	my $discovered_by = $self->_discovered_by->{$key};
	return $discovered_by ? $discovered_by->@* : ();
}

sub is_discovered ($self, $key)
{
	return exists $self->_discovered_by->{$key};
}

sub _discover_general ($self, $actor, $discovered_hash, $discovered_by, $resource)
{
	state $radius = Game::Config->discover_radius;

	my $actor_id = $actor->id;
	my $discovered = $discovered_hash->{$actor_id} //= {};
	my %not_found = $discovered->%*;
	my @objects_new;

	my $found_objects = $self->find_in_radius($actor->variables->xy, $radius);

	foreach my $found_id ($found_objects->@*) {
		push $discovered_by->{$found_id}->@*, $actor_id;    # found_id may not be a player, but that's fine
		next if delete $not_found{$found_id};    # player is already aware of this object
		next if $found_id eq $actor_id;    # player can't discover himself

		$discovered->{$found_id} = true;
		push @objects_new, $found_id;
	}

	foreach my $not_found_id (keys %not_found) {
		delete $discovered->{$not_found_id};
		$resource->add_old_object($not_found_id);
	}

	return \@objects_new;
}

sub _discover_actors ($self, $actor, $new_objects, $resource)
{
	my $location = $self->location;
	my @not_actors;

	foreach my $found_id ($new_objects->@*) {
		my $found = $location->get_actor($found_id);
		if (!$found) {
			push @not_actors, $found_id;
			next;
		}

		$resource->add_new_actor($found);
		$self->queue('signal_actor_appeared', $actor, $found);
	}

	return \@not_actors;
}

sub _discover ($self)
{
	$self->_set_discovered_by({});
	my $discovered_hash = $self->_discovered;
	my $discovered_by = $self->_discovered_by;

	foreach my $actor ($self->location->get_players->@*) {
		my $resource = Resource::Discovery->new;
		my $objects_new = $self->_discover_general($actor, $discovered_hash, $discovered_by, $resource);

		for my $method (qw(_discover_actors)) {
			$objects_new = $self->$method($actor, $objects_new, $resource);
		}

		$self->send_to_player($actor->id, $resource) if $resource->should_send;
	}

	$self->resolve_queue;

	return;
}

sub actors_info ($self, $requesting_actor_id, $wanted_actors)
{
	my @wanted_actors_data;
	my $discovered_actors = $self->_discovered->{$requesting_actor_id};
	my $all_actors = $self->location->actors;

	foreach my $actor_id ($wanted_actors->@*) {
		push @wanted_actors_data, $all_actors->{$actor_id}
			if $discovered_actors->{$actor_id} || $actor_id eq $requesting_actor_id;
	}

	return \@wanted_actors_data;
}

after BUILD => sub ($self, @) {
	$self->_add_action(1 => '_discover');
};

after signal_player_left => sub ($self, $actor) {
	delete $self->_discovered->{$actor->id};
};

