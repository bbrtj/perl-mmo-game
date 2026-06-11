package Tiled::Parser;

use My::Moose;
use Path::Tiny;
use Tiled::Map;
use XML::PugiXML;
use Encode qw(decode);

use header;

sub _read_properties ($self, $node)
{
	my @nodes = $node->select_nodes('.//properties/property');

	return map {
		$_->attr('name')->value => $_->attr('value')->value
	} @nodes;
}

sub _read_map_string ($self, $map_node, $width, $height)
{
	my $mapdata = join "\n",
		(Tiled::Map::TILE_VOID x $width)
		x $height;

	foreach my $layer ($map_node->select_nodes('.//layer')) {
		my %properties = $self->_read_properties($layer);

		next unless $properties{terrain_type};
		my $tile
			= $properties{terrain_type} eq 'void'
			? Tiled::Map::TILE_VOID
			: $properties{terrain_type} eq 'wall' ? Tiled::Map::TILE_WALL
			: Tiled::Map::TILE_TERRAIN
			;

		my $data = $layer->child('data')->text;
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

	my $parser = XML::PugiXML->new;
	$parser->load_string(decode 'utf-8', $contents);

	my $map = $parser->child('map');
	my %args = map { $_ => $map->attr($_)->value }
		qw(width height tilewidth tileheight);

	$args{path} = $path;
	$args{map} = $self->_read_map_string($map, $args{width}, $args{height});

	my $map_object = Tiled::Map->new(%args);

	foreach my $object_layer ($map->select_nodes('.//objectgroup')) {
		my %properties = $self->_read_properties($object_layer);

		next unless ($properties{private} // '') eq 'true';

		my $type = $object_layer->attr('name')->value;
		foreach my $object ($object_layer->select_nodes('.//object')) {
			$map_object->objects->add_object(
				$type,
				{map { $_->name => $_->value } $object->attrs},
				{$self->_read_properties($object)}
			);
		}
	}

	return $map_object;
}

sub groom_map ($self, $path)
{
	my $contents = path("assets/$path")->slurp;

	my $parser = XML::PugiXML->new;
	$parser->load_string(decode 'utf-8', $contents);
	my %args;

	foreach my $layer_property ($parser->select_nodes('/map//layer//properties')) {
		$layer_property->parent->remove_child($layer_property);
	}

	foreach my $object_layer ($parser->select_nodes('/map//objectgroup')) {
		my %properties = $self->_read_properties($object_layer);
		$_->parent->remove_child($_) for $object_layer->select_nodes('.//properties');

		next unless ($properties{private} // '') eq 'true';
		$object_layer->parent->remove_child($object_layer);
	}

	return $parser->to_string;
}

sub _groom_tileset ($self, $map_path, $path)
{
	my $contents = path("assets/$map_path")->parent->child($path)->slurp;

	my $parser = XML::PugiXML->new;
	$parser->load_string(decode 'utf-8', $contents);

	my $image = $parser->select_node('/tileset/image');
	my $source = $image->attr('source')->value;
	$source =~ s{\.\./client/data/}{}x;

	$image->set_attr(source => $source);
	return $parser->to_string;
}

sub groom_tilesets ($self, $path)
{
	my $contents = path("assets/$path")->slurp;

	my $parser = XML::PugiXML->new;
	$parser->load_string(decode 'utf-8', $contents);

	my %tileset_contents;
	foreach my $tileset ($parser->select_nodes('/map//tileset')) {
		my $tileset_path = $tileset->attr('source')->value;

		$tileset_contents{$tileset_path} = $self->_groom_tileset($path, $tileset_path);
	}

	return %tileset_contents;
}

