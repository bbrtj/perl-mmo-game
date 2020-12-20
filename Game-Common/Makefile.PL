use 5.030001;
use ExtUtils::MakeMaker;

WriteMakefile(
	NAME => 'Game::Common',
	VERSION_FROM => 'lib/Game/Common.pm',

	TEST_REQUIRES => {
		'Test::More' => 0,
		'Test::DB' => 0,
		'Test::Exception' => 0,
	},

	PREREQ_PM => {
		'Moo' => 0,
		'Moose' => 0,
		'Type::Tiny' => 0,
		'Type::Libraries' => 0,
		'Types::UUID' => 0,
		'Types::DateTime' => 0,
		'Type::EmailAddress' => 0,
		'Mojolicious' => 0,
		'Mojo::Pg' => 0,
		'Minion' => 0,
		'namespace::clean' => 0,
		'Syntax::Keyword::Try' => 0,
		'DateTime::Format::Pg' => 0,
	},
	AUTHOR => 'brtastic',
);
