package Game::Lore::Location;

use My::Moose;
use Types;

use header;

extends 'Game::Lore';

use constant prefix => 'LOC';


package Game::Lore::LocationData {
	use My::Moose;
	use Types;
	use Form::Tiny::Utils qw(trim);

	use header;

	extends 'Game::LoreData';

	has 'x' => (
		is => 'ro',
		writer => 'set_x',
		isa => Types::Num,
	);

	has 'y' => (
		is => 'ro',
		writer => 'set_y',
		isa => Types::Num,
	);

	has 'size_x' => (
		is => 'ro',
		writer => 'set_size_x',
		isa => Types::Int,
	);

	has 'size_y' => (
		is => 'ro',
		writer => 'set_size_y',
		isa => Types::Int,
	);

	has 'map' => (
		is => 'ro',
		writer => '_set_map',
		isa => Types::ArrayRef[Types::ArrayRef[Types::Bool]],
	);

	has 'connections' => (
		is => 'ro',
		default => sub { [] },
	);

	has '+parent' => (
		isa => Types::InstanceOf['Game::Lore::Area'],
	);

	sub set_map ($self, $map_str)
	{
		my @map_lines = grep { /\S/ } map { trim $_ } split "\n", $map_str;
		my @map_size = (length $map_lines[0], scalar @map_lines);
		my @map;

		for my $line (@map_lines) {
			my @bools = map { scalar(/\w/) } split '', $line;

			die "invalid map size on line ($line)"
				if @bools != $map_size[0];

			push @map, \@bools;
		}

		$self->_set_map(\@map);
		$self->set_size_x($map_size[0]);
		$self->set_size_y($map_size[1]);
		return;
	}
}

