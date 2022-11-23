package Game::Server::Role::Discovery;

use My::Moose::Role;
use Game::Config;
use Resource::Discovery;

use header;

requires qw(
	location_data
	find_in_radius
	send_to_actor
);

has cached '_discovered_actors' => (
	isa => Types::HashRef [Types::HashRef],
	default => sub { {} },
);

sub _discover_players ($self, $actor, $found_objects, $resource)
{
	my %found_prev = %{$self->_discovered_actors->{$actor} // {}};
	my %not_found = %found_prev;

	my @new;
	my @old;

	foreach my $found ($found_objects->@*) {
		next if $found eq $actor;
		next unless $found isa 'Unit::Actor';

		if ($found_prev{$found}) {
			delete $not_found{$found};
		}
		else {
			$found_prev{$found} = $found;
			push @new, $found->player->id;
		}
	}

	foreach my $not_found (values %not_found) {
		push @old, $not_found->player->id;
		delete $found_prev{$not_found};
	}

	if (@new || @old) {
		$resource->new_players(@new);
		$resource->old_players(@old);
		$self->_discovered_actors->{$actor} = \%found_prev;
		return !!1;
	}

	return !!0;
}

sub _discover ($self, $elapsed_time)
{
	state $radius = Game::Config->config->{discover_radius};

	foreach my $actor ($self->location_data->actors->@*) {
		my $resource = Resource::Discovery->new({});
		my $variables = $actor->variables;
		my $should_send = 0;

		my $found_objects = $self->find_in_radius($variables->pos_x, $variables->pos_y, $radius);

		for my $method (qw(_discover_players)) {
			$should_send = $self->$method($actor, $found_objects, $resource) || $should_send;
		}

		$self->send_to_actor($actor, $resource) if $should_send;
	}

	return;
}

after BUILD => sub ($self, @) {
	$self->_add_action(4 => '_discover');
};

