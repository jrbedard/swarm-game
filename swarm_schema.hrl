%% @author J.R. Bedard <jrbedard@gmail.com>
%% @copyright 2008 jrbedard.

%% @doc Swarm Mnesia Schema.



%% city/lgn/lat mappings

-record(city, {id,
			   name,
			   lat,
			   lgn}).


%% SWARM records

-record(person, {id,
                 name,
				 password,
				 online,
                 status,
  			 	 image_url,
                 bio,
                 website,
                 karma,
				 location,
				 event}).

-record(location, {id, 
               	   name,
				   type,
				   image_url,
				   description,
				   private,
				   lgn,
				   lat,
				   address,
				   website,
				   rating}).

-record(event, {id,
				name,
                type,
				image_url,
				description,
				date_start,
				date_end,
				website,
				rating}).


%% Person somewhere real-time

-record(p_at_loc, {person,
				   datetime,
				   lat,
				   lgn,
				   loc_id,
				   e_id}).

%% Person historic
-record(p_wasat_loc, {person,
					  datetime,
					  lat,
				   	  lgn,
				      loc_id,
				      e_id}).
				

%% Event somewhere

-record(e_at_loc, {event,
				   datetime,
                   lat,
				   lgn,
				   loc_id}).

%% todo: was


%% Image of something

-record(img_at_loc, {image_url,
					 lat,
					 lgn,
					 loc_id,
					 e_id}).


%% Favorites

-record(p_fav_p, {person,
				  p_id}).

-record(p_fav_loc, {person,
				    loc_id}).

-record(p_fav_e, {person,
				  e_id}).

