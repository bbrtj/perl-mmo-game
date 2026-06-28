package CLI::Command::export_maps;

use My::Moose;
use Path::Tiny qw(cwd);
use JSON::MaybeXS qw(encode_json);
use Tiled::Parser;
use Utils;

use header;

BEGIN { extends 'CLI::Command' }

use constant description => 'Exports all maps in the system for the client';
use constant usage => __PACKAGE__->extract_usage;

has field 'base_path' => (
	default => sub { cwd->child('client')->child('data')->child('maps') },
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
		my %map = (
			SizeX => $loc->map->size_x,
			SizeY => $loc->map->size_y,
		);

		push @locations, {
			Id => $loc->id,
			PosX => $loc->pos_x,
			PosY => $loc->pos_y,
			Area => $loc->parent->id,
			ConnectedTo => [
				map { $_->id } $loc->connections->@*
			],
			Map => \%map,
		};
	}

	my @locations_mapped = map {
		{file => $self->id_to_file($_->{Id}), id => $_->{Id}}
	} @locations;

	$self->base_path->child('index.json')->spew(encode_json {index => \@locations_mapped});
	my $path = $self->base_path->child('meta')->mkdir;

	foreach my $item (@locations) {
		my $fliename = $self->id_to_file($item->{Id});
		$path->child("$fliename.json")->spew(encode_json $item);
	}

	return;
}

sub _groom_maps ($self, $locs)
{
	my $parser = Tiled::Parser->new;
	$self->base_path->mkdir;

	foreach my $loc ($locs->@*) {
		my $filename = $self->id_to_file($loc->id);
		my $assets_path = $loc->map->map_object->path;

		my $map = $parser->groom_map($assets_path);
		$self->base_path->child("$filename.tmx")->spew($map);
	}

	return;
}

sub _copy_tilesets ($self, $locs)
{
	my $parser = Tiled::Parser->new;
	$self->base_path->mkdir;

	foreach my $loc ($locs->@*) {
		my $filename = $self->id_to_file($loc->id);
		my $assets_path = $loc->map->map_object->path;

		my %tilesets = $parser->groom_tilesets($assets_path);
		foreach my $tileset_path (keys %tilesets) {
			my $content = $tilesets{$tileset_path};

			$self->base_path->child($tileset_path)->spew($content);
		}
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
	$self->_copy_tilesets(\@locs);
	$self->_generate_metadata(\@locs);

	say "done, generated in $path";

	return;
}

__END__

=head1 SYNOPSIS

	Usage: APPLICATION export-maps

