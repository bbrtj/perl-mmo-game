package Game::Checks::Map;

use Exporter qw(import);
use X::Pub;

use header;

our @EXPORT_OK = qw(
	can_move_to
	can_see
);

sub can_move_to ($actor, $map, $x, $y)
{
	X::Pub->raise
		unless $actor;

	# TODO: only if the actor is close to the wall
	X::Pub::CheckFailed->raise(Err::CANNOT_MOVE)
		unless $map->check_can_be_accessed($x, $y);

	return;
}

# get tile sides in range as an array, like this:
# [ ]|[ ]|[ ]|[ ]
# (sides marked with |)
sub _get_tile_sides ($from, $to)
{
	return $to >= $from
		? ($from + 1 .. $to)
		: ($to + 1 .. $from)
		;
}

sub _can_see_inner ($map, $sx, $sy, $ex, $ey)
{
	my $coeff_x = ($ey - $sy) / ($ex - $sx);

	my $partial_y = $sy - $sx * $coeff_x;
	foreach my $pos_x (_get_tile_sides($sx, $ex)) {
		my $pos_y = $partial_y + $pos_x * $coeff_x;

		return false unless $map->check_within_map($pos_x, $pos_y)
			&& $map->check_within_map($pos_x - 1, $pos_y);
	}

	my $partial_x = $sx - $sy / $coeff_x;
	foreach my $pos_y (_get_tile_sides($sy, $ey)) {
		my $pos_x = $partial_x + $pos_y / $coeff_x;

		return false unless $map->check_within_map($pos_x, $pos_y)
			&& $map->check_within_map($pos_x, $pos_y - 1);
	}

	return true;
}

sub can_see
{
	X::Pub::CheckFailed->raise(Err::NOT_IN_LOS)
		unless _can_see_inner(@_);
}

