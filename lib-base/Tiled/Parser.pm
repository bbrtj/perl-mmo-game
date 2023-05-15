package Tiled::Parser;

use My::Moose;
use Mojo::DOM;
use Mojo::File qw(path);
use Tiled::Map;

use header;

sub _read_properties ($self, $dom)
{
	my @properties_raw = $dom
		->find('properties>property')
		->map('attr')
		->each
		;

	return map {
		$_->{name} => $_->{value}
	} @properties_raw;
}

sub _read_map_string ($self, $map_dom, $width, $height)
{
	my $mapdata = join "\n",
		(Tiled::Map::TILE_VOID x $width)
		x $height;

	foreach my $layer ($map_dom->find('layer')->each) {
		my %properties = $self->_read_properties($layer);

		next unless $properties{terrain_type};
		my $tile
			= $properties{terrain_type} eq 'void' ? Tiled::Map::TILE_VOID
			: $properties{terrain_type} eq 'wall' ? Tiled::Map::TILE_WALL
			: Tiled::Map::TILE_TERRAIN
			;

		my $data = $layer->at('data')->text;
		$data =~ s{ +}{}g;
		$data =~ s{[1-9]\d*}{$tile}g;
		$data =~ s{,}{}g;
		$data =~ s{^\s}{}sg;

		my $last = 0;
		while ((my $nextpos = index $data, $tile, $last) >= 0) {
			$last = index $data, '0', $nextpos;
			$last = length($data) - 1 unless $last >= 0;
			my $length = $last - $nextpos;
			substr $mapdata, $nextpos, $length,
				substr $data, $nextpos, $length;
		}
	}

	return $mapdata;
}

sub parse_map ($self, $path)
{
	my $contents = path("assets/$path")->slurp;

	my $dom = Mojo::DOM->new($contents);
	my %args;

	my $map = $dom->at('map');
	for my $attr ($map->attr) {
		$args{$_} = $attr->{$_}
			for qw(width height tilewidth tileheight);
	}

	$args{path} = $path;
	$args{map} = $self->_read_map_string($map, $args{width}, $args{height});

	my $map_object = Tiled::Map->new(%args);

	foreach my $object_layer ($map->find('objectgroup')->each) {
		my %properties = $self->_read_properties($object_layer);

		next unless ($properties{private} // '') eq 'true';

		my $type = $object_layer->attr->{name};
		foreach my $object ($object_layer->find('object')->each) {
			$map_object->objects->add_object(
				$type,
				$object->attr,
				{ $self->_read_properties($object) }
			);
		}
	}

	return $map_object;
}

sub groom_map ($self, $path)
{
	my $contents = path("assets/$path")->slurp;

	my $dom = Mojo::DOM->new($contents);
	my %args;

	my $map = $dom->at('map');

	foreach my $layer ($map->find('layer')->each) {
		$_->remove for $layer->find('properties')->each;
	}

	foreach my $object_layer ($map->find('objectgroup')->each) {
		my %properties = $self->_read_properties($object_layer);
		$_->remove for $object_layer->find('properties')->each;

		next unless ($properties{private} // '') eq 'true';
		$object_layer->remove;
	}

	return $dom->to_string;
}

sub _groom_tileset ($self, $map_path, $path)
{
	my $contents = path("assets/$map_path")->dirname->child($path)->slurp;

	my $dom = Mojo::DOM->new($contents);
	my $tileset = $dom->at('tileset');
	my $image = $tileset->at('image');
	my $source = $image->attr->{source};
	$source =~ s{\.\./client/data/}{}x;

	$image->attr(source => $source);
	return $dom->to_string;
}

sub groom_tilesets ($self, $path)
{
	my $contents = path("assets/$path")->slurp;

	my $dom = Mojo::DOM->new($contents);
	my $map = $dom->at('map');

	my %tileset_contents;
	foreach my $tileset ($map->find('tileset')->each) {
		my $tileset_path = $tileset->attr->{source};

		$tileset_contents{$tileset_path} = $self->_groom_tileset($path, $tileset_path);
	}

	return %tileset_contents;
}

