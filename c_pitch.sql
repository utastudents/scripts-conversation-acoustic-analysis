CREATE TABLE [dbo].[c_pitch](
	subcorpus_cd [varchar](10) NOT NULL,
	conv_id [varchar](10) NOT NULL,
	ch [tinyint] NOT NULL,
	freq [float] NULL,
	pt [int] NOT NULL,
PRIMARY KEY CLUSTERED 
(
	subcorpus_cd ASC,
	conv_id ASC,
	ch ASC,
	pt ASC
)
)