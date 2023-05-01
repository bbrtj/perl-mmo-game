package CLI::export_maps;

use My::Moose -constr;
use Mojo::File qw(path);
use Mojo::JSON qw(encode_json);
use Utils;

use header;

extends 'Mojolicious::Command';

use constant description => 'Exports all maps in the system for the client';
sub usage ($self) { return $self->extract_usage }

sub id_to_file ($self, $lore_id)
{
	$lore_id =~ s/\./__/g;
	return lc $lore_id;
}

# TODO: redo this to work with tiled maps
sub run ($self, $language = undef)
{
	my $repo = DI->get('lore_data_repo');
	my @locs = values $repo->load_all_named('Game::Lore::Location')->%*;

	my @locations;

	foreach my $loc (@locs) {
		my $data = $loc->data;

		my %map = (
			SizeX => $data->map->size_x,
			SizeY => $data->map->size_y,
			Coords => [
				map {
					+{
						$_->type ne $_->contents ? (TileContents => $_->contents) : (),
						!$data->map->is_terrain($_) ? (TileTerrain => $data->map->guess_terrain($_)) : (),
						TileType => $_->type,

					}
				} map {
					$_->@*
				} $data->map->coordinates->@*
			]
		);

		push @locations, {
			Id => $loc->id,
			PosX => $data->pos_x,
			PosY => $data->pos_y,
			Area => $data->parent->id,
			ConnectedTo => [
				map { $_->id } $data->connections->@*
			],
			Map => \%map,
		};
	}

	my @locations_mapped = map {
		{file => $self->id_to_file($_->{Id}), id => $_->{Id}}
	} @locations;

	my $output = path->child('client')->child('data')->child('mapindex.json');
	$output->spurt(encode_json {index => \@locations_mapped});

	my $path = path->child('client')->child('data')->child('maps')->child('meta');
	$path->remove_tree->make_path;
	foreach my $item (@locations) {
		my $fliename = $self->id_to_file($item->{Id});
		my $map = $path->child("$fliename.json");
		$map->spurt(encode_json $item);
	}

	say "done, generated in $output";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-maps

