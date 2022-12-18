package Game::Object::Map;

use My::Moose;

use header;

extends 'Game::TileMap';

my $legend = __PACKAGE__->new_legend(characters_per_tile => 2)
	->add_wall('##')
	->add_wall('#^' => 'building')
	->add_void('@@')
	->add_void('@w' => 'water')
	->add_terrain('__' => 'level_terrain')
	->add_object('npcs', 'n1' => 'npc1')
	;

has extended 'legend' => (
	default => sub { $legend },
);

sub is_terrain ($self, $object)
{
	return $self->get_class_of_object($object) eq 'terrain';
}

sub guess_terrain ($self, $object)
{
	my ($x, $y) = ($object->x, $object->y);

	my %found;
	foreach my $check_x (-1 .. 1) {
		foreach my $check_y (-1 .. 1) {
			my $tile = $self->coordinates->[$check_x][$check_y];
			next if !$self->is_terrain($tile);
			next if $tile->is_wall || $tile->is_void;
			$found{$tile->type}++;
		}
	}

	# die "unable to guess terrain at $x:$y"
	return 'level_terrain'
		if !%found;

	my @sorted = sort { $found{$b} <=> $found{$a} } keys %found;
	return $sorted[0];
}

