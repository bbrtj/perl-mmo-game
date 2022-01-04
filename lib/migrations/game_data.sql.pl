use Mojo::File qw(curfile path);
use Encode qw(decode);

use strict;
use warnings;

my @files = map { curfile->dirname->to_string . "/game_data/$_.sql" } qw(
	gd_lores
	gd_languages
	gd_lore_names
	gd_lore_descriptions

	gd_ability_attributes
	gd_ability_effect_types
	gd_abilities

	gd_classes
	gd_class_abilities

	gd_ratings
	gd_statistics
);
my $contents = join "\n", map { decode("UTF-8", path($_)->slurp) } @files;

# replication requires superuser access
return <<SQL;

-- 2 up

$contents

-- 2 down

SQL
