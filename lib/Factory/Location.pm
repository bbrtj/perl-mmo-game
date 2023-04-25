package Factory::Location;

use My::Moose;
use Factory::Actor;
use Schema::Utils qw(fetch_all);
use all 'Unit';

use header;

extends 'Factory';

has injected 'lore_data_repo';

sub fetch ($self, $location_id)
{
	my $location = $self->lore_data_repo->load($location_id);

	# NOTE: not needed, but may come in handy in the future
	# my $rs = $self->dbc->resultset('Character')->search(
	# 	{'player.online' => 1, 'variables.location_id' => $location_id},
	# 	{
	# 		prefetch => [qw(player variables)],
	# 	}
	# );
	# my $characters = fetch_all($rs);

	return {location => $location};
}

sub create ($self, $results)
{
	return Unit::Location->new(
		lore => $results->{location},
	);
}

