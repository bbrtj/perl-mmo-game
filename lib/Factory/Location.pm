package Factory::Location;

use My::Moose;
use Factory::Actor;
use Unit::Location;
use Schema::Utils qw(fetch_all);
use DI;

use header;

extends 'Factory';

has 'lore' => (
	is => 'ro',
	default => sub { DI->get('lore_data') }
);

sub fetch ($self, $location_id)
{
	my $location = $self->lore->load($location_id);

	my $rs = $self->dbc->resultset('Character')->search(
		{'player.online' => 1, 'variables.location_id' => $location_id},
		{
			prefetch => [qw(player variables)],
		}
	);

	return {location => $location, characters => fetch_all($rs)};
}

sub create ($self, $results)
{
	return Unit::Location->new(
		location => $results->{location},
		actors => [map { Factory::Actor->create($_) } $results->{characters}->@*],
	);
}

