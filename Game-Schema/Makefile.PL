use 5.030001;
use ExtUtils::MakeMaker;

WriteMakefile(
	NAME => 'Game::Schema',
	VERSION_FROM => 'lib/Game/Schema.pm',
	PREREQ_PM => {
		'Game::Common' => 0,
		'DBIx::Class' => 0,
	},
	AUTHOR => 'brtastic',
);
