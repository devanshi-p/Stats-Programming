#########################################################
## Stat 202A - Final Project
## Author: Devanshi Patel
## Date : 12/10/2017
## Description: This script implements a two layer neural network in Tensorflow
#########################################################
#mera wala which is actually wrong
#############################################################
## INSTRUCTIONS: Please fill in the missing lines of code
## only where specified. Do not change function names,
## function inputs or outputs. You can add examples at the
## end of the script (in the "Optional examples" section) to
## double-check your work, but MAKE SURE TO COMMENT OUT ALL
## OF YOUR EXAMPLES BEFORE SUBMITTING.
##
## Very important: Do not use the function "os.chdir" anywhere
## in your code. If you do, I will be unable to grade your
## work since Python will attempt to change my working directory
## to one that does not exist.
#############################################################

############################################################################
## Implement a two layer neural network in Tensorflow to classify MNIST digits ##
############################################################################

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Train a two layer neural network to classify the MNIST dataset ##
## Use Relu as the activation function for the first layer. Use Softmax as the activation function for the second layer##
## z=Relu(x*W1+b1) ##
## y=Softmax(z*W2+b2)##
# Use cross-entropy as the loss function#
# Tip: be careful when you initialize the weight and bias parameters.
## You only need to install the CPU version of Tensorflow on your laptop, which is much easier.
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
from __future__ import absolute_import
from __future__ import division
from __future__ import print_function

import argparse
import sys
import os
os.environ["CUDA_VISIBLE_DEVICES"]=""

from tensorflow.examples.tutorials.mnist import input_data
import tensorflow as tf
import matplotlib.pyplot as plt
import numpy as np

FLAGS = None

hidden_nodes = 500
num_classes = 10
batch_size = 100


def main(_):
  # Import data
  mnist = input_data.read_data_sets(FLAGS.data_dir, one_hot=True)
  x = tf.placeholder(tf.float32, [None, 784])
  y = tf.placeholder(tf.float32, )

  # Create the model
  #######################
  ## FILL IN CODE HERE ##
  #######################

  w1 = tf.get_variable('weights1', [784, hidden_nodes], initializer=tf.random_normal_initializer(stddev=0.3))
  b1 = tf.get_variable('bias1', [1, ], initializer=tf.random_normal_initializer(stddev=0.3))
  layer1 = tf.matmul(x, w1) + b1
  l1 = tf.nn.relu(layer1)

  w2 = tf.get_variable('weights2', [hidden_nodes, num_classes], initializer=tf.random_normal_initializer(stddev=0.3))
  b2 = tf.get_variable('bias2', [1, ], initializer=tf.random_normal_initializer(stddev=0.3))
  layer2 = tf.matmul(l1, w2) + b2
  l2 = tf.nn.softmax(layer2)

  prediction = l2
  crossentropy = tf.reduce_mean(tf.nn.softmax_cross_entropy_with_logits(logits=prediction, labels=y))

  optimizer = tf.train.AdamOptimizer().minimize(crossentropy)
  num_epochs = 100
  acc = np.repeat(0., num_epochs)
  loss = np.repeat(0., num_epochs)

  with tf.Session() as sess:
    tf.global_variables_initializer().run()
    for epoch in range(num_epochs):
      epoch_loss = 0
      for i in range(int(mnist.train.num_examples / batch_size)):
        epoch_x, epoch_y = mnist.train.next_batch(batch_size)
        _, c = sess.run([optimizer, crossentropy], feed_dict={x: epoch_x, y: epoch_y})
        epoch_loss += c

      correct = tf.equal(tf.argmax(prediction, 1), tf.argmax(y, 1))
      accuracy = tf.reduce_mean(tf.cast(correct, tf.float32))
      acc[epoch] = sess.run(accuracy, feed_dict={x: mnist.test.images, y: mnist.test.labels})
      loss[epoch] = epoch_loss
      print('Epoch', epoch, ',Accuracy : ', acc[epoch], ',Loss :', epoch_loss)


if __name__ == '__main__':
  parser = argparse.ArgumentParser()
  parser.add_argument('--data_dir', type=str, default='/tmp/tensorflow/mnist/input_data',
                      help='Directory for storing input data')
  FLAGS, unparsed = parser.parse_known_args()
  tf.app.run(main=main, argv=[sys.argv[0]] + unparsed)