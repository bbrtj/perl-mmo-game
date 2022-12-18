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

sub run ($self, $language = undef)
{
	my $repo = DI->get('lore_data_repo');
	my @locs = values $repo->load_all_named('Game::Lore::Location')->%*;

	my @locations;

	foreach my $loc (@locs) {
		my $data = $loc->data;

		my %map = (
			size_x => $data->map->size_x,
			size_y => $data->map->size_y,
			coordinates => [
				map {
					{
						$_->type ne $_->contents ? (contents => $_->contents) : (),
						!$data->map->is_terrain($_) ? (terrain => $data->map->guess_terrain($_)) : (),
						type => $_->type,

					}
				} map {
					$_->@*
				} $data->map->coordinates->@*
			]
		);

		push @locations, {
			id => $loc->id,
			pos_x => $data->pos_x,
			pos_y => $data->pos_y,
			area => $data->parent->id,
			connected_to => [
				map { $_->id } $data->connections->@*
			],
			map => \%map,
		};
	}

	my @locations_mapped = map {
		{ file => $self->id_to_file($_->{id}), id => $_->{id} }
	} @locations;

	my $output = path->child('client')->child('data')->child('mapindex.json');
	$output->spurt(encode_json \@locations_mapped);

	my $path = path->child('client')->child('data')->child('maps');
	$path->remove_tree->make_path;
	foreach my $item (@locations) {
		my $fliename = $self->id_to_file($item->{id});
		my $map = $path->child("$fliename.json");
		$map->spurt(encode_json $item);
	}

	say "done, generated in $output";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-maps

