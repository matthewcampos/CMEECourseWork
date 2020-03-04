# CMEE 2019 HPC excercises R code HPC run code proforma

rm(list=ls()) # good practice 
source("mlc19_HPC_2019_main.R")
# it should take a faction of a second to source your file
# if it takes longer you're using the main file to do actual simulations
# it should be used only for defining functions that will be useful for your cluster run and which will be marked automatically

# do what you like here to test your functions (this won't be marked)
# for example
species_richness(c(1,4,4,5,1,6,1))
# should return 4 when you've written the function correctly for question 1

# you may also like to use this file for playing around and debugging
# but please make sure it's all tidied up by the time it's made its way into the main.R file or o
init_community_max(7)
init_community_min(4)

species_richness(init_community_min(5)) #1
species_richness(init_community_max(5)) #5

choose_two(4)

neutral_step(c(10,5,13))
neutral_generation(c(10,5,13,10))
neutral_time_series(community=init_community_max(7),20)

question_8()

neutral_step_speciation(c(10,5,13),0.8)
neutral_generation_speciation(c(10,5,13,20),0.3)
neutral_time_series_speciation(community=init_community_max(7),0.3,15)

question_12()

species_abundance(c(1,5,3,6,5,6,1,1))
octaves(c(100,64,63,5,4,3,2,2,1,1,1,1))
sum_vect(c(1,3),c(1,0,5,2))

question_16()

cluster_run(speciation_rate = 0.1,size = 100,wall_time = 3,interval_rich = 1,interval_oct = 10,length_burn_in_generations = 200,output_file_name = "my_test_file_1.rda")
process_cluster_results()

question_21()
question_22()
chaos_game()
turtle(c(0,0),pi/4,2)
elbow(c(0,0),pi/4,2)
spiral(c(0,3),pi/4,2)
draw_spiral()

tree(c(2.5,0),pi/2,2)
draw_tree()
fern(c(0,0),pi/4,0.7)
draw_fern()
fern2(c(2.5,0),1,pi/2,1) 
draw_fern2()

Challenge_A()
Challenge_B()
Challenge_C()
Challenge_D()
Challenge_E()
Challenge_F()
