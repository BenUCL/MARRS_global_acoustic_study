
### Set Up

This guide explains how to set up Perch with GPU usage on Linux. The system used for this guide is a Dell XPS 15 with an Nvidia RTX 4060 GPU, running Ubuntu 24.04.

#### Step 1: Create the Conda Environment

First, use the `perch_conda_env.yml` file to create the Conda environment. This will install TensorFlow 2.15.0 alongside the correct CUDA libraries.

```bash
cd /marrs_acoustics/code/setup/env_setup
conda env create -f perch_conda_env.yml
```

#### Step 2: Verify TensorFlow Installation and GPU Detection

Activate the Conda environment and run a test script to check if TensorFlow is installed correctly and if the GPU is recognized. The script will print out the number of GPUs found and the TensorFlow version.

```bash
conda activate perch_conda_env
python tf_test.py
```

If no GPU is found, follow the advice [here](https://github.com/tensorflow/tensorflow/issues/63362#issuecomment-2016019354):

```bash
export NVIDIA_DIR=$(dirname $(dirname $(python -c "import nvidia.cudnn;print(nvidia.cudnn.__file__)")))
export LD_LIBRARY_PATH=$(echo ${NVIDIA_DIR}/*/lib/ | sed -r 's/\s+/:/g')${LD_LIBRARY_PATH:+:${LD_LIBRARY_PATH}}
```

Rerun the test script to ensure the GPU is now recognized by TensorFlow:

```bash
python tf_test.py
```

#### Step 3: Install the Poetry Environment

Next, install the Poetry environment for Perch. This will add additional packages required by Perch. Note that TensorFlow has been removed from the `pyproject.toml` file used by Perch.

```bash
cd /marrs_acoustics/code
poetry install
```

If you receive a message indicating that the lock file has been edited and needs updating, run the following command and then retry the Poetry installation:

```bash
poetry lock
poetry install
```

#### Step 4: Verify Installation

The Conda environment should now have the `chirp` package and other dependencies installed. To verify the installation, check the version of `chirp`:

```bash
python -c "import chirp; print(chirp.__version__)"
```

Finally, verify the TensorFlow version and GPU detection again. Note that TensorFlow may have updated to version 2.16.2:

```bash
python tf_test.py
```

If the test script shows that 1 or more GPUs are found, the setup is successful and GPU support is enabled.