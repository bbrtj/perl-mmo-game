package CLI::export_maps;

use My::Moose -constr;
use Mojo::File qw(path);
use Mojo::JSON qw(encode_json);
use Tiled::Parser;
use Utils;

use header;

extends 'Mojolicious::Command';

use constant description => 'Exports all maps in the system for the client';
sub usage ($self) { return $self->extract_usage }

has field 'base_path' => (
	default => sub { path->child('client')->child('data')->child('maps') },
);

sub id_to_file ($self, $lore_id)
{
	$lore_id =~ s/\./__/g;
	return lc $lore_id;
}

sub _generate_metadata ($self, $locs)
{
	my @locations;

	foreach my $loc ($locs->@*) {
		my $data = $loc->data;

		my %map = (
			SizeX => $data->map->size_x,
			SizeY => $data->map->size_y,
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

	$self->base_path->child('index.json')->spurt(encode_json {index => \@locations_mapped});
	my $path = $self->base_path->child('meta')->make_path;

	foreach my $item (@locations) {
		my $fliename = $self->id_to_file($item->{Id});
		$path->child("$fliename.json")->spurt(encode_json $item);
	}

	return;
}

sub _groom_maps ($self, $locs)
{
	my $parser = Tiled::Parser->new;
	$self->base_path->make_path;

	foreach my $loc ($locs->@*) {
		my $filename = $self->id_to_file($loc->id);
		my $assets_path = $loc->data->map->map_object->path;

		my $map = Tiled::Parser->groom_map($assets_path);
		$self->base_path->child("$filename.tmx")->spurt($map);
	}

	return;
}

sub run ($self)
{
	my $repo = DI->get('lore_data_repo');
	my @locs = values $repo->load_all_named('Game::Lore::Location')->%*;

	# clear old maps before generation
	my $path = $self->base_path;
	$path->remove_tree;

	$self->_groom_maps(\@locs);
	$self->_generate_metadata(\@locs);

	say "done, generated in $path";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-maps

